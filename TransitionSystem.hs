module TS where

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



-- Circuit generation part:

type OneHot = [Ref]

type OneHotFlop = L (OneHot, OneHot -> L ())


-- TODO: could also take an initial state of variables as input, if that's not
-- in the Synchronisation
-- Question to self: what does above TODO mean?
-- ins are misc. input, like the "uncontrollable" we talked about
transitionsystem :: Synchronisation -> OneHot -> [Ref] -> L Ref
transitionsystem s ev ins
 | (length ev) /= (length $ allEvents s) = error "Event array wrong length"
 | otherwise = do 

      -- create state variables
     varFlops <- sequence [ flop Nothing | var <- allBoolVars s ]
     let (varRefs, nextVars) = unzip varFlops

     -- Map from BoolVar to Ref
     let varMap v = fromJust $ lookup v (zip (allBoolVars s) varRefs)
     -- Map from BoolVar to update function (Ref -> L ())
     let nextValMap v = fromJust $ lookup (varMap v) varFlops 
     
     -- create location state variables
     locFlops <- sequence [ locationOH aut | aut <- automata s ]
     let (locs, nextLocs) = unzip locFlops
     
     -- bind eventMap
     let eventMap e = fromJust $ lookup e (zip (allEvents s) ev)
     
     -- create circuits and find updated values for each event
     let
      ts = 
        [ ( [ (t, loc)
            | (a, loc) <- (automata s) `zip` locs,
              t <- transitions a,
              e == (event t) ]
          , eventMap e )
        | e <- allEvents s ]
     let (_, evRefs) = unzip ts
     
     eventCircuits <- sequence $ map (eventToLava varMap) ts
     let (eventUds, evErrs) = unzip eventCircuits
     
     
     let
      updatesByVar = groupBy ((==) `on` snd)
        [ ((newVal, evRef), var)
        | (udList, evRef) <- zip eventUds evRefs,
          (var, newVal) <- udList
        ]
     
     let
      uds1 = [ (updatesToLava refPairs, head vars)
             | uds <- updatesByVar,
               let (refPairs, vars) = unzip uds
             ]
     let
      (uds2, vars) = unzip uds1
     uds3 <- sequence uds2
     let
      (newVals, udErrs) = unzip uds3
      
     sequence_ $ zipWith ($) (map nextValMap vars) newVals
     
     err1 <- orl evErrs
     err2 <- orl udErrs
     
     err <- or2 err1 err2
     
     return err
     

locationOH :: Automaton -> OneHotFlop
locationOH a = oneHotFlops (1, nbrLocations a)

-- Input is (hot_value, #values)
oneHotFlops :: (Int, Int) -> OneHotFlop
oneHotFlops (val, max)
-- TODO: add case (maybe flag val=-1) for an all-maybe flop? Or would that
-- almost always come up as an illegal one-hot array?
  | 1 <= val && val <= max =
    let (L m0) = sequence [ flop (Just (i==val))
                          | i <- [1..max] ]
    in L (\n0 -> let (tups, n1, gs1) = m0 n0
                     (ins, outs)     = unzip tups
                     outApp          = (zipWith ($) outs)
                  in ((ins, sequence_ . outApp), n1, gs1))    
  | otherwise = error "oneHotFlops: index out of bounds"


type RefMap k = k -> Ref

-- Input is state var map, transition, eventIsFired, isInLocation
-- Output is (possible updates, transFired, error)
transToLava :: (RefMap BoolVar) -> Transition -> Ref -> Ref ->
  L ([(BoolVar, Ref)],Ref,Ref)
transToLava svm t ef isinl =
  do     

     -- check whether the event is fired
     transFired <- and2 ef isinl
          
     -- Check all the guards
     gs <- sequence $ map (guardToLava svm) (guards t)
     clearedGuards <- orl gs
     

     blocked <- and2 transFired (neg clearedGuards)
     
     -- Compute possible updates
     udNew <- sequence [ updateToLava update | update <- updates t ]
     let uds = zip (map uvar (updates t)) udNew
     
     return (uds, transFired, blocked)
   
guardToLava :: (RefMap BoolVar) -> Guard -> L Ref
guardToLava svm g = case (gval g) of
                         (True) -> return $ svm $ gvar g
                         (False) -> return $ neg $ svm $ gvar g
                         
-- TODO: when using non-constant updates, edit this function to
-- take more Refs as arguments - perhaps a (RefMap BoolVar)
updateToLava :: Update -> L Ref
updateToLava u = case (uval u) of
                      (True) -> return tt
                      (False) -> return ff


-- This function represents the Lava circuit generation corresponding
-- to one event; i.e. all transitions firing on that event.
-- Input are: varmap, (transition-location-pairs, eventIsFired)
-- Output is (updates, error)
eventToLava :: (RefMap BoolVar) -> ([(Transition, OneHot)], Ref) ->
  L ([(BoolVar, Ref)], Ref)
eventToLava bvm (tlps, ef) =
  do
     transOutputs <- sequence
       [ transToLava bvm t ef isinl
       | (t,l) <- tlps,
         let isinl = l!!(start t) ]
         
     let (allUpdates, allIsFireds, transErrs) = unzip3 transOutputs
     
     err1 <- orl transErrs
     
     let
      updatesByVar = groupBy ((==) `on` snd)
        [ ((newVal, isFired), var)
        | (udList, isFired) <- zip allUpdates allIsFireds,
          (var, newVal) <- udList
        ]
          
     let
      uds1 = [ (updatesToLava refPairs, head vars)
             | uds <- updatesByVar,
               let (refPairs, vars) = unzip uds
             ]
     let
      (uds2, vars) = unzip uds1
     uds3 <- sequence uds2
     let
      (newVals, udErrs) = unzip uds3
      uds4 = zip vars newVals
      
     err2 <- orl udErrs
     
     err <- or2 err1 err2
     
     return (uds4, err)
     

-- Input: pairs of isFired and newVals
-- Output: newval, error
-- Desired logic: if only one isFired is true, that newVal should go through.
-- If more than one, error is true.
updatesToLava :: [(Ref, Ref)] -> L (Ref, Ref)
updatesToLava pairs = 
 do
    let (gates, vals) = unzip pairs
    one  <- multi_xor gates
    let err = neg one
    
    outs <- sequence $ map (uncurry and2) pairs
    out <- orl outs
    
    return (out,err)
     

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x


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

s1 = foldl synchronise emptySynch [exAut1, exAut3]

eventOH :: [Event] -> OneHot
eventOH = map Pos




