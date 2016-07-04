module TransitionSystem where

import Data.Maybe
import Data.Function
import qualified Data.Map as Map
import Data.List
import qualified Control.Monad as C
import Circuit

--------------------


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
  }
  deriving ( Show )


type Predicate = (Location, [Guard])


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
events a = nub $ map event (transitions a)
-- TODO: nub is quadratic – we can do better

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








