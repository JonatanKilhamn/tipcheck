module TransitionSystem where

import Data.Maybe
import Data.Function
import qualified Data.Map as Map
import Data.List
import qualified Control.Monad as C
import Circuit
import Lava

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


--type State = (Location, [VarVal])


data Automaton
  = Aut
  { autName :: Name
  , nbrLocations :: Int
  , nbrBoolVars  :: Int
  , nbrEvents    :: Int
  , transitions :: [Transition]
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

boolVars :: Automaton -> [Event]
boolVars a = nub $ concat $ map boolVars' (transitions a)
 where boolVars' t = (map gvar (guards t)) ++ (map uvar (updates t))


synchronise :: Synchronisation -> Automaton -> Synchronisation
synchronise s a =
  Synch {automata = a:(automata s)
        , allEvents = union (allEvents s) (events a)
        , allBoolVars = union (allBoolVars s) (boolVars a)
        }

emptySynch :: Synchronisation
emptySynch = Synch {automata = []
                   , allEvents = []
                   , allBoolVars = []
                   }



--
-- Test instances
--

g0 :: Guard
g0 = Guard {gvar = undefined, gval = undefined}

l1, l2 :: Location
l1 = 0
l2 = 1

e1, e2, e3, e4 :: Event
e1 = "a"
e2 = "b"
e3 = "c"
e4 = "d"

bvar1, bvar2 :: BoolVar
bvar1 = "x"
bvar2 = "y"

u1 :: Update
u1 = Update {uvar = bvar1, uval = False}

t1, t2 :: Transition
t1 = Trans {start=l1,event=e1,guards=[g0],updates=[],end=l2}
t2 = Trans {start=l2,event=e2,guards=[g0],updates=[],end=l1}

aut1 :: Automaton
aut1 = Aut {autName = "foo",
            nbrLocations = 2,
            nbrBoolVars = 0,
            nbrEvents = 2,
            transitions = [t1,t2]}


-- Example automata


globalEventList :: [Event]
globalEventList = [e1,e2,e3,e4]


ex1t1, ex1t2 :: Transition
ex1t1 = Trans {start=l1,event=e1,guards=[],updates=[],end=l2}
ex1t2 = Trans {start=l2,event=e2,guards=[],updates=[],end=l1}

ex2t1, ex2t2, ex2t3, ex2t4 :: Transition
ex2t1 = Trans {start=l1,event=e1,guards=[],updates=[],end=l2}
ex2t2 = Trans {start=l1,event=e2,guards=[],updates=[],end=l2}
ex2t3 = Trans {start=l2,event=e3,guards=[],updates=[],end=l1}
ex2t4 = Trans {start=l2,event=e4,guards=[],updates=[],end=l1}


exAut1, exAut2, exAut3 :: Automaton
exAut1 = Aut { autName = "a1"
             , nbrLocations = 2
             , nbrBoolVars = 0
             , nbrEvents = 2
             , transitions = [ex1t1, ex1t2]
             }

exAut2 = Aut { autName = "a2"
             , nbrLocations = 5
             , nbrBoolVars = 0
             , nbrEvents = 4
             , transitions = [ex2t1, ex2t2, ex2t3, ex2t4]
             }

exAut3 = Aut { autName = "a3"
             , nbrLocations = 2
             , nbrBoolVars = 0
             , nbrEvents = 2
             , transitions = [ex1t1, ex1t2]
             }

s1 = foldl synchronise emptySynch [exAut1, exAut3]



