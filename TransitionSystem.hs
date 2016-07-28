module TransitionSystem where

import Data.Maybe
import Data.Function
import qualified Data.Map as M
import Data.List
import qualified Control.Monad as C
import Circuit
import qualified Data.Set as S
import Test.QuickCheck

--------------------

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go S.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `S.member` s then go s xs
                                    else x : go (S.insert x s) xs

-- Only boolean state variables so far
-- Only constants for the right-hand sides of guards and updates

type Event = Name
type VarName = Name
--type Value = Int
-- Location names only need to be unique within the automaton
type Location = Name

data IntExpr
 = IntConst Int
 | Plus IntExpr IntExpr
 | Minus IntExpr IntExpr
 | IntVar VarName
  deriving ( Show )

data Guard = GInt BinaryPred VarName IntExpr
  deriving ( Show )

guardVarName :: Guard -> VarName
guardVarName (GInt _ x _) = x


data BinaryPred
 = Equals
 | LessThan
 | LessThanEq
 | GreaterThan
 | GreaterThanEq
  deriving ( Show )
 


data Update
 = AssignInt VarName IntExpr
  deriving ( Show )
 
updateVarName :: Update -> VarName
updateVarName (AssignInt x _) = x

 

{-data UnaryOp
 = Inv -- TODO: which unary operators are there?
-}

data Variable
  = Variable
  { lower :: Int
  , upper :: Int
  , initial :: Int
  }
  deriving ( Show, Eq )

isBooleanVariable :: Variable -> Bool
isBooleanVariable v = (lower v == 0) && (upper v == 1)

{-data Guard
  = Guard
  { gvar  :: Var
  , gval  :: Value
  }
  deriving ( Show, Eq )-}

{-data Update
  = Update
  { uvar :: Var
  , uval :: Value
  }
  deriving ( Show, Eq )-}


data Transition
  = Trans
  { start   :: Location
  , event   :: Event
  , guards  :: [Guard]
  , updates :: [Update]
  , end     :: Location
  , uncontrollable :: Bool
  }
  deriving ( Show )



type Predicate = (Location, [Guard])


-- TODO: current structure does not prohibit one automaton
-- from having several transitions from the same location,
-- firing on the same event – i.e. nondeterminism. In the
-- circuit translation, such a situation would be treated
-- as an error, when two transitions try to update the same
-- location variable. 
data Automaton
  = Aut
  { autName :: Name
  , locations :: S.Set Location
  , transitions :: [Transition]
  , marked :: [Predicate]
  , initialLocation:: Location
  }
  deriving ( Show )



data Synchronisation
  = Synch
  { automata :: [Automaton]
  , allEvents   :: [Event]
  , allVars :: M.Map VarName Variable
  --, allIntVars :: M.Map BoolVar (Value)
  --, synchLog :: String
  }
 deriving ( Show )

events :: Automaton -> [Event]
events a = ordNub $ map event (transitions a)


getAllVars :: Automaton -> M.Map VarName Variable
getAllVars a = M.fromList $ zip varNames (repeat unknownVar)
 where varNames = ordNub $ concat $ map varNames' (transitions a)
       varNames' t = (map guardVarName (guards t)) ++ (map updateVarName (updates t))
       unknownVar = Variable {lower = 0, upper = 1, initial = 0}


synchronise :: Automaton -> Synchronisation -> Synchronisation
synchronise a s =
  s {automata = a:(automata s)
    , allEvents = union (allEvents s) (events a)
    , allVars = M.unionWith takeFirst (allVars s) (getAllVars a)
    }
 where takeFirst = flip seq

setDefault :: (VarName, Int) -> Synchronisation -> Synchronisation
setDefault (bv, n) s =
 let v = (allVars s) M.! bv in
  s {allVars = M.update (\_ -> Just v {initial = n}) bv (allVars s)
    }

setRangeMax :: (VarName, Int) -> Synchronisation -> Synchronisation
setRangeMax (bv, n) s =
 let v = (allVars s) M.! bv in
  s {allVars = M.update (\_ -> Just v {upper = n}) bv (allVars s)
    }

setRangeMin :: (VarName, Int) -> Synchronisation -> Synchronisation
setRangeMin (bv, n) s =
 let v = (allVars s) M.! bv in
  s {allVars = M.update (\_ -> Just v {lower = n}) bv (allVars s)
    }


emptySynch :: Synchronisation
emptySynch = Synch {automata = []
                   , allEvents = []
                   , allVars = M.empty
                   --, synchLog = ""
                   }


setEventUncontrollable :: Event -> Synchronisation -> Synchronisation
setEventUncontrollable e s =
 s {automata = updatedAuts}
  where
     updatedAuts = map updateAut (automata s)
     updateAut a = a {transitions = map updateTrans (transitions a)}
     updateTrans t = if event t == e
                     then t {uncontrollable = True}
                     else t


-- TODO: checkSynchronisation, which checks that all initial values lie between the bounds

