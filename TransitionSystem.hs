module TS where

import Data.Maybe
import qualified Data.Map as Map
import Data.List
import qualified Control.Monad as C
import Circuit
import Lava

--------------------


-- All these are indices only
type Location = Int
type BoolVarInd = Int
type EventInd = Int


-- Only boolean state variables so far

data Guard
  = Guard
  { gvar  :: BoolVarInd
  , gval  :: Bool
  }
  deriving ( Show, Eq )

data Update
  = Update
  { uvar :: BoolVarInd
  , uval :: Bool
  }
  deriving ( Show, Eq )


data Transition
  = Trans
  { start   :: Location
  , event   :: EventInd
  , guards  :: [Guard]
  , updates :: [Update]
  , end     :: Location
  }
  deriving ( Show )


type State = (Location, [VarVal])


data Automaton
  = Aut
  { nbrLocations :: Int
  , nbrBoolVars  :: Int
  , nbrEvents    :: Int
  , transitions :: [Transition]
  }
  deriving ( Show )


type Event = Name
type BoolVar = Name


data ContextAutomaton
   = CA
   {automaton :: Automaton
   , events   :: [Event]
   , boolVars :: [BoolVar]
   }
  deriving ( Show )


data Synchronisation
  = Synch
  { automata :: [ContextAutomaton]
  , allEvents   :: [Event]
  , allBoolVars :: [BoolVar]
  }
 deriving ( Show )


synchronise :: ContextAutomaton -> Synchronisation -> Synchronisation
synchronise ca s =
  Synch {automata = ca:(automata s)
        , allEvents = union (allEvents s) (events ca)
        , allBoolVars = union (allBoolVars s) (boolVars ca)
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


locationOH :: ContextAutomaton -> L [(Ref, Ref -> L ())]
locationOH ca = oneHotFlops (0, nbrLocations (automaton ca))


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

e1, e2 :: EventInd
e1 = 0
e2 = 1

u1 :: Update
u1 = Update {uvar = 0, uval = False}

t1, t2 :: Transition
t1 = Trans {start=l1,event=e1,guards=[g0],updates=[],end=l2}
t2 = Trans {start=l2,event=e2,guards=[g0],updates=[],end=l1}

aut1 :: Automaton
aut1 = Aut {nbrLocations = 2,
            nbrBoolVars = 0,
            nbrEvents = 2,
            transitions = [t1,t2]}

caut1 :: ContextAutomaton
caut1 = CA {automaton = aut1, events = ["foo","bar"], boolVars = ["x"]}


bvar1, bvar2 :: BoolVar
bvar1 = "x"
bvar2 = "y"


-- Example automata

e3, e4 :: EventInd
e3 = 2
e4 = 3

globalEventList :: [Event]
globalEventList = ["a","b","c","d"]



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

exCA1, exCA2, exCA3 :: ContextAutomaton
exCA1 = CA
        { automaton = exAut1
        , events = map (\x -> globalEventList!!x) [0,2]
        , boolVars = []
        }
exCA2 = CA
        { automaton = exAut2
        , events = globalEventList
        , boolVars = []
        }
exCA3 = CA
        { automaton = exAut3
        , events = map (\x -> globalEventList!!x) [1,3]
        , boolVars = []
        }




