module TS where

import Data.Maybe
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
  { nbrLocations :: Int
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

--events :: Automaton -> [Event]
--events a = foldl (union $ event) [] (transitions a)

--boolVars :: Automaton -> [Event]
--boolVars a = foldl (union . (guards)) [] (transitions a)



synchronise :: Automaton -> Synchronisation -> Synchronisation
synchronise a s =
  Synch {automata = a:(automata s)
        , allEvents = undefined--union (allEvents s) (events a)
        , allBoolVars = undefined--union (allBoolVars s) (boolVars a)
        }

emptySynch :: Synchronisation
emptySynch = Synch {automata = []
                   , allEvents = []
                   , allBoolVars = []
                   }




-- Circuit generation part:

type OneHot = [Ref]


-- TODO: could also take an initial state of variables as input, if that's not
-- in the Synchronisation
-- Question to self: what does above TODO mean?
-- rs are misc. input, like the "uncontrollable" we talked about
transitionsystem :: Synchronisation -> OneHot -> [Ref] -> L [Ref]
transitionsystem s rs event =    
  do -- create state variables
     varFlops <- sequence [ flop Nothing | var <- allBoolVars s ]
     let (vars, nextVars) = unzip varFlops
     -- create location state variables
     locFlops <- sequence [ locationOH aut | aut <- automata s ]
     
     
     -- create circuits corresponding to each transition
     
     
     return undefined
     



-- Input is event, location, state vars, transition
-- Output is (possible updates, error)
transToLava :: OneHot -> OneHot -> [Ref] -> Transition -> L ([Ref],Ref)
transToLava e l svs t = undefined {-
  do
     -- check for the right event
     let fire = e!!(startEvent)
     return undefined
-}
 where
   startEvent = start t


locationOH :: Automaton -> L [(Ref, Ref -> L ())]
locationOH a = oneHotFlops (0, nbrLocations a)


-- Input is (hot_value, #values)
oneHotFlops :: (Int, Int) -> L [(Ref, Ref -> L ())]
oneHotFlops (val, max)
  | 1 <= val && val <= max = do
    sequence [ flop (if i==val then Just True else Just False)
             | i <- [1..max] ]
  | otherwise = error "oneHotFlops: index out of bounds"




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
aut1 = Aut {nbrLocations = 2,
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
exAut1 = Aut { nbrLocations = 2
             , nbrBoolVars = 0
             , nbrEvents = 2
             , transitions = [ex1t1, ex1t2]
             }

exAut2 = Aut { nbrLocations = 2
             , nbrBoolVars = 0
             , nbrEvents = 4
             , transitions = [ex2t1, ex2t2, ex2t3, ex2t4]
             }

exAut3 = Aut { nbrLocations = 2
             , nbrBoolVars = 0
             , nbrEvents = 2
             , transitions = [ex1t1, ex1t2]
             }




