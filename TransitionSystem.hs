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

events :: Automaton -> [Event]
events a = nub $ map event (transitions a)

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



-- Circuit generation part:

type OneHot = [Ref]

type OneHotMap k = k -> Ref

type OneHotFlop = L (OneHot, OneHot -> L ())



-- TODO: could also take an initial state of variables as input, if that's not
-- in the Synchronisation
-- Question to self: what does above TODO mean?
-- rs are misc. input, like the "uncontrollable" we talked about
transitionsystem :: Synchronisation -> OneHot -> [Ref] -> L [Ref]
transitionsystem s event rs =    
  do -- create state variables
     varFlops <- sequence [ flop Nothing | var <- allBoolVars s ]
     let (varRefs, nextVars) = unzip varFlops
     let varMap = Map.fromList (zip (allBoolVars s) varRefs)
     -- create location state variables
     locFlops <- sequence [ locationOH aut | aut <- automata s ]
     
     
     -- create circuits corresponding to each transition
     --(transToLava locFlops
     
     return undefined
     


locationOH :: Automaton -> OneHotFlop
locationOH a = oneHotFlops (0, nbrLocations a)



-- Input is location, event map, state var map, transition
-- Output is (possible updates, error)
transToLava :: OneHot -> (OneHotMap Event) -> (OneHotMap BoolVar) -> Transition ->
  L ([BoolVar -> Ref],Ref)
transToLava l em svm t =
  do
     -- check for the right event
     let eventFired = em transitionEvent
     -- Check whether we're in the right location
     let isInStartLocation = l!!(startLocation)
     -- Check all the guards
     gs <- sequence $ map (guardToLava svm) (guards t)
     clearedGuards <- orl gs
     
     -- Compute possible updates
     let uds = []
     
     -- Compute error
     -- is it possible to find an error on this level?
     let err = ff

     return (uds,err)

 where
   transitionEvent = event t
   startLocation = start t
   
guardToLava :: (BoolVar -> Ref) -> Guard -> L Ref
guardToLava svm g = case (gval g) of
                         (True) -> return $ svm $ gvar g
                         (False) -> return $ neg $ svm $ gvar g
                         




-- Input is (hot_value, #values)
oneHotFlops :: (Int, Int) -> OneHotFlop
oneHotFlops (val, max)
  | 1 <= val && val <= max =
    let (L m0) = sequence [ flop (if i==val then Just True else Just False)
                          | i <- [1..max] ]
    in L (\n0 -> let (tups, n1, gs1) = m0 n0
                     (ins, outs)     = unzip tups
                     outApp          = (zipWith ($) outs)
                  in ((ins, sequence_ . outApp), n1, gs1))    
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




