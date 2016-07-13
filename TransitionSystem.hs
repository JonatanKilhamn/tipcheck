module TransitionSystem where

import Data.Maybe
import Data.Function
import qualified Data.Map as M
import Data.List
import qualified Control.Monad as C
import Circuit
import qualified Data.Set as S

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
type BoolVar = Name
type Value = Bool
-- Location names only need to be unique within the automaton
type Location = Name

data Guard
  = Guard
  { gvar  :: BoolVar
  , gval  :: Value
  }
  deriving ( Show, Eq )

data Update
  = Update
  { uvar :: BoolVar
  , uval :: Value
  }
  deriving ( Show, Eq )


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
  , allBoolVars :: M.Map BoolVar Value
  }
 deriving ( Show )

events :: Automaton -> [Event]
events a = ordNub $ map event (transitions a)


boolVars :: Automaton -> M.Map BoolVar Value
boolVars a = M.fromList $ zip varNames (repeat False)
 where varNames = ordNub $ concat $ map varNames' (transitions a)
       varNames' t = (map gvar (guards t)) ++ (map uvar (updates t))


synchronise :: Automaton -> Synchronisation -> Synchronisation
synchronise a s =
  Synch {automata = a:(automata s)
        , allEvents = union (allEvents s) (events a)
        , allBoolVars = M.unionWith takeFirst (allBoolVars s) (boolVars a)
        }
 where takeFirst = flip seq

setDefault :: (BoolVar, Value) -> Synchronisation -> Synchronisation
setDefault (bv, v) s = s {allBoolVars = M.update (\_ -> Just v) bv (allBoolVars s)
                         }
-- TODO update the default value

emptySynch :: Synchronisation
emptySynch = Synch {automata = []
                   , allEvents = []
                   , allBoolVars = M.empty
                   }









