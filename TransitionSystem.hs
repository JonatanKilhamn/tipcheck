module TransitionSystem where

import Data.Maybe
import Data.Function
import qualified Data.Map as M
import Data.List
import qualified Control.Monad as C
import Control.Applicative
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

-- Only unary-encoded integer-valued state variables so far

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
  deriving ( Eq )

instance Show IntExpr where
 show (IntConst i) = show i
 show (Plus ie1 ie2) = "("++show ie1++" + "++show ie2++")"
 show (Minus ie1 ie2) = "("++show ie1++" - "++show ie2++")"
 show (IntVar x) = show x

iePlus :: IntExpr -> IntExpr -> IntExpr
iePlus (IntConst x) (IntConst y) = IntConst (x+y)
iePlus a b = Plus a b

varNames :: IntExpr -> [VarName]
varNames (IntVar vn) = [vn]
varNames (Plus ie1 ie2) = union (varNames ie1) (varNames ie2)
varNames (Minus ie1 ie2) = union (varNames ie1) (varNames ie2)
varNames _ = []

data Guard = GInt BinaryPred VarName IntExpr
           | Top
           | Bottom
           | GOr [Guard]
           | GAnd [Guard]
           | GNot Guard
  deriving ( Eq )

instance Show Guard where
 show (GInt bp vn ie) = (show vn) ++ (show bp) ++ (show ie)
 show Top = "True"
 show Bottom = "False"
 show (GOr gs) = "ANY "++show gs
 show (GAnd gs) = "ALL "++show gs
 show (GNot g) = "~"++show g

guardVarName :: Guard -> VarName
guardVarName (GInt _ x _) = x
guardVarName (GNot g) = guardVarName g

guardVarNames :: Guard -> [VarName]
guardVarNames (GInt _ x exp) = union [x] (varNames exp)
guardVarNames (GNot g) = guardVarNames g
guardVarNames (GAnd gs) = foldl union [] (map guardVarNames gs)
guardVarNames (GOr gs) = foldl union [] (map guardVarNames gs)
guardVarNames Top = []
guardVarNames Bottom = []


disjunctionGuard :: [Guard] -> Guard
disjunctionGuard (g:[]) = g
disjunctionGuard gs
 | elem Top gs = Top
 | otherwise = GOr (filter (/= Bottom) gs)


data BinaryPred
 = Equals
 | NEquals
 | LessThan
 | LessThanEq
 | GreaterThan
 | GreaterThanEq
  deriving ( Eq )

instance Show BinaryPred where
 show Equals = "=="
 show NEquals = "/="
 show LessThan = "<"
 show LessThanEq = "<="
 show GreaterThan = ">"
 show GreaterThanEq = ">="




data Update
 = AssignInt VarName IntExpr
  deriving ( Show, Eq )
 
updateVarName :: Update -> VarName
updateVarName (AssignInt x _) = x

updateVarNames :: Update -> [VarName]
updateVarNames (AssignInt x exp) = union [x] (varNames exp)


{-data UnaryOp
 = Inv -- TODO: which unary operators are there?
-}

data Variable
  = Variable
  { lower :: Int
  , upper :: Int
  , initial :: Int
  }
  deriving ( Eq )

instance Show Variable where
  show var = "["++(show $ lower var)++".."++(show $ upper var)++"], init:"++(show $ initial var)

isBooleanVariable :: Variable -> Bool
isBooleanVariable v = (lower v == 0) && (upper v == 1)

mkVar :: (Int,Int,Int) -> Variable
mkVar (l,u,i) = Variable {lower=l,upper=u,initial=i}


data Transition
  = Trans
  { start   :: Location
  , event   :: Event
  , guards  :: [Guard]
  , updates :: [Update]
  , end     :: Location
  --, uncontrollable :: Bool
  }
  deriving ( Eq )

instance Show Transition where
  show trans = unlines $
    [ (show $ event trans) ++ ": " ++
      (show $ start trans) ++ "-->" ++ (show $ end trans)
    ] ++
    [ "GUARDS"
    | not (null (guards trans))
    ] ++
    [ "  " ++ (show g)
    | g <- guards trans
    ] ++
    [ "UPDATES"
    | not (null (updates trans))
    ] ++
    [ "  " ++ (show u)
    | u <- updates trans
    ]

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
  , initialLocation :: Location
  , uncontrollable :: [Event]
  }

instance Show Automaton where
  show aut = unlines $
    [ "NAME: " ++ autName aut ] ++
    [ "TRANSITIONS:"
    | not (null (transitions aut))
    ] ++
    [ "  " ++ (show t)
    | t <- transitions aut
    ] ++
    [ "MARKED:"
    | not (null (marked aut))
    ] ++
    [ "  " ++ (if (gs == []) then (show (l,True)) else show (l,gs))
    | (l,gs) <- marked aut
    ] ++
    [ "INITIAL: " ++ (show $ initialLocation aut)
    ] ++
    [ "UNCONTROLLABLE: " ++ (show $ uncontrollable aut)
    ]



data Synchronisation
  = Synch
  { automata :: [Automaton]
  , allEvents   :: [Event]
  , allVars :: M.Map VarName Variable
  --, allIntVars :: M.Map BoolVar (Value)
  --, synchLog :: String
  }

instance Show Synchronisation where
  show synch = unlines $
    [ "=== SYNCHRONISATION ==="] ++
    [ "#AUTOMATA: " ++ (show $ length $ automata synch) ] ++
    [ "AUT. No "++ (show i) ++ " " ++ (show a)
    | (a,i) <- zip (automata synch) [1..]
    ] ++
    [ "ALL VARIABLES IN SYNCH: "
    | not (null (allVars synch))
    ] ++
    [ "  " ++ name ++ ": " ++ (show var)
    | (name, var) <- M.assocs $ allVars synch
    ] ++
    [ "UNCONTROLLABLE EVENTS: "
    | not (null (getAllUncontrollable synch))
    ] ++
    [ "  " ++ name
    | (name) <- getAllUncontrollable synch
    ]

events :: Automaton -> [Event]
events a = ordNub $ map event (transitions a)


getAllVars :: Automaton -> M.Map VarName Variable
getAllVars a = M.fromList $ zip varNames (repeat unknownVar)
 where varNames = ordNub $ concat $ map varNames' (transitions a)
       varNames' t = concat $ (map guardVarNames (guards t)) ++ (map updateVarNames (updates t))
       unknownVar = Variable {lower = 0, upper = 3, initial = 0}

setUncontrollable :: (Event, Bool) -> Automaton -> Automaton
setUncontrollable (e,b) aut =
 if b
 then aut { uncontrollable =
             if e `elem` events aut
             then union [e] (uncontrollable aut)
             else uncontrollable aut
          }
 else aut { uncontrollable = filter (/=e) (uncontrollable aut) }


synchronise :: Automaton -> Synchronisation -> Synchronisation
synchronise a s =
  s {automata = a:(automata s)
    , allEvents = union (allEvents s) (events a)
    , allVars = M.unionWith takeFirst (allVars s) (getAllVars a)
    }
 where takeFirst = flip seq

setDefault :: (VarName, Int) -> Synchronisation -> Synchronisation
setDefault (name, n) s =
  s {allVars = M.adjust (\v -> v {initial = n}) name (allVars s)
    }

setRangeMax :: (VarName, Int) -> Synchronisation -> Synchronisation
setRangeMax (name, n) s =
  s {allVars = M.adjust (\v -> v {upper = n}) name (allVars s)
    }

setRangeMin :: (VarName, Int) -> Synchronisation -> Synchronisation
setRangeMin (name, n) s =
  s {allVars = M.adjust (\v -> v {lower = n}) name (allVars s)
    }

setVars :: [(VarName, Variable)] -> Synchronisation -> Synchronisation
setVars vs s = foldr (\(name, var) -> \sy -> sy {allVars = M.insert name var (allVars sy)}) s vs


addUpdate :: Update -> Transition -> Transition
addUpdate u t = t { updates = union [u] (updates t) }


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
     updateAut = setUncontrollable (e,True)

isEventUncontrollable :: Event -> Synchronisation -> Bool
isEventUncontrollable e =
 (elem e) . getAllUncontrollable

getAllUncontrollable :: Synchronisation -> [Event]
getAllUncontrollable =
 (foldr union []) . ((map uncontrollable) . automata)


oneHotBool :: (Int, Int) -> [Bool]
oneHotBool (val, max) = [ if (i == val) then True else False | i <- [1..max] ]

eventInput :: Event -> Synchronisation -> [Bool]
eventInput e s = [ if e==ev then True else False | ev <- allEvents s ]
  


-- TODO: checkSynchronisation, which checks that all initial values lie within the bounds

