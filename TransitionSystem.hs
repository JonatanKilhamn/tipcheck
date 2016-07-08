module TransitionSystem where

import Data.Maybe
import Data.Function
import qualified Data.Map as Map
import Data.List
import qualified Control.Monad as C
import Circuit
import qualified Data.Set as Set

--------------------

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

-- Only boolean state variables so far
-- Only constants for the right-hand sides of guards and updates

type Event = Name
type BoolVar = Name
type Location = Int

data Guard
  = Guard
  { gvar  :: BoolVar
  , gval  :: Bool
  }
  deriving ( Show, Eq )

data Update
  = Update
  { uvar :: BoolVar
  , uval :: Bool
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
  , nbrLocations :: Int
  , transitions :: [Transition]
  , marked :: [Predicate]
  }
  deriving ( Show )



data Synchronisation
  = Synch
  { automata :: [Automaton]
  , allEvents   :: [Event]
  , allBoolVars :: [BoolVar]
  }
 deriving ( Show )

events :: Automaton -> [Event]
events a = ordNub $ map event (transitions a)


boolVars :: Automaton -> [Event]
boolVars a = nub $ concat $ map boolVars' (transitions a)
 where boolVars' t = (map gvar (guards t)) ++ (map uvar (updates t))


synchronise :: Automaton -> Synchronisation -> Synchronisation
synchronise a s =
  Synch {automata = a:(automata s)
        , allEvents = union (allEvents s) (events a)
        , allBoolVars = union (allBoolVars s) (boolVars a)
        }

emptySynch :: Synchronisation
emptySynch = Synch {automata = []
                   , allEvents = []
                   , allBoolVars = []
                   }








