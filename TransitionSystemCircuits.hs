module TransitionSystemCircuits where

import Data.Maybe
import Data.Function
import Data.List
import Control.Monad
import Circuit
import Lava
import TransitionSystem
import Control.Monad.State

--------------------



type OneHot = [Ref]
type Bin = [Ref]
type Un = [Ref]

type OneHotFlop = (OneHot, OneHot -> L ())


data CState k = CS { origVal   :: k
                   , latestVal :: k
                   , hasUpdated :: Ref
                   , hasError  :: Ref
                   }
  deriving ( Show )

newState :: k -> CState k
newState a = CS { origVal = a
                , latestVal = a
                , hasUpdated = ff
                , hasError = ff
                }

type CLocation = CState OneHot
type CBoolVar = CState Ref
type CEvent = Ref


type LocationsMap = [(Name, CLocation)]

type BoolVarMap = [(BoolVar, CBoolVar)]

type EventMap = [(Event, CEvent)]


data PartialSynchCircuit
  = PSC
  { locMap :: LocationsMap
  , boolVarMap :: BoolVarMap
  , eventMap   :: EventMap
  , globalError :: Ref
  }

type LocationsRefMap = [(Name, OneHot)]

type BoolVarRefMap = [(BoolVar, Ref)]

type EventRefMap = [(Event, Ref)]

type MarkedRefMap = [(Name, Ref)]

data SynchCircuit
  = SynchC
  { locRefs :: LocationsRefMap
  , boolVarRefs :: BoolVarRefMap
  , eventRefs   :: EventRefMap
  , markedRefs :: MarkedRefMap
  , anyError :: Ref
  }
 deriving ( Show )



processSystem :: Synchronisation -> [Ref] -> L SynchCircuit
processSystem s ins =
   do
     -- input processing
     evRefs <- sequence [ input | x <- allEvents s]

     let autNames = map autName (automata s)
         boolVarNames = allBoolVars s
         evm = zip (allEvents s) evRefs

     -- create location state variables
     locFlops <- sequence [ locationOH aut | aut <- automata s ]
     let locRefs = map fst locFlops

      -- create state variables
     varFlops <- sequence [ flop0 | var <- boolVarNames ]
     
     -- create big clumsy object to pass around
     let auts = [ (name, (newState loc))
                | ((loc, _), name) <- zip locFlops autNames ]
         bvs = zip (allBoolVars s) (map (newState . fst) varFlops)
         state = PSC { locMap = auts
                     , boolVarMap = bvs
                     , eventMap   = evm
                     , globalError = ff
                     }
     
     -- process each automaton
     state1 <- foldM processAutomaton state (automata s)
     
     -- set the updated location values
     let newLocs = map (latestVal . snd) (locMap state1)
     sequence_ $ zipWith ($)
       (map snd locFlops) newLocs
       
     -- set the updated variable values
     let newBoolVars = map (latestVal . snd) (boolVarMap state1)
     sequence_ $ zipWith ($)
       (map snd varFlops) newBoolVars
     
     -- compute which automata are in marked states
     markedRefs <- sequence $ map (checkMarked state1)
                                  (automata s)
     let marked = zip autNames markedRefs
     
     -- compute errors
     locErr <- orl (map (hasError . snd) (locMap state1))
     varErr <- orl (map (hasError . snd) (boolVarMap state1))
     localError <- or2 locErr varErr
     
     validInput <- isOH evRefs
     error1 <- or2 localError (neg validInput)
     finalError <- or2 error1 (globalError state1)
     
     -- output
     let circuit = SynchC { locRefs = zip autNames newLocs
                          , boolVarRefs = zip boolVarNames newBoolVars
                          , eventRefs = evm
                          , markedRefs = marked
                          , anyError = finalError
                          }
     return circuit



processAutomaton :: PartialSynchCircuit -> Automaton ->
  L PartialSynchCircuit
processAutomaton state a =
 do
  let trans = transitions a
      bvm = boolVarMap state
      evm = eventMap state
      auts = locMap state
      an = autName a
      cloc = fromJust $ lookup an auts
      getEventRef t = fromJust $ lookup (event t) evm

  -- create refs signifying which transitions are enabled
  enableds <- sequence $ map (isEnabledTransition cloc bvm) trans
  -- refs signifying which transitions are fired
  fireds <- sequence $ zipWith and2 (map getEventRef trans) enableds
  
  let transAndFireds = zip trans fireds
  
  -- update locations
  cloc' <- foldM locationUpdate cloc transAndFireds
  let auts' = replaceAt (an, cloc') auts
  
  -- update state variables
  bvm' <- foldM varUpdates bvm transAndFireds
      
  -- find errors stemming from a blocking automaton
  autError <- isBlocked evm a enableds
  globalError' <- or2 autError (globalError state)
  
  return state { locMap = auts'
               , boolVarMap = bvm'
               , globalError = globalError'
               }

isEnabledTransition :: CLocation -> BoolVarMap -> Transition ->
  L Ref
isEnabledTransition cloc bvm t =
 do
  let startLocRef = (origVal cloc) !! (start t)
  
  -- check all the guards
  let bvrm = zip (map fst bvm) (map (origVal . snd) bvm)
  gs <- sequence $ map (guardToLava bvrm) (guards t)
  clearedGuards <- andl gs
  and2 startLocRef clearedGuards


locationUpdate :: CLocation -> (Transition, Ref) -> L CLocation
locationUpdate cloc (t, fired) =
 do
  let i1 = start t
      i2 = end t
      startLocRef = (origVal cloc) !! i1
  -- update locations
  -- input: lastval, hasUpdated, hasError, shouldUpdate, newVal
  let lastVal = map ((latestVal cloc) !!) [i1, i2]
      hasUd = hasUpdated cloc
      hasErr = hasError cloc
      newVal = [ff, tt]
  (lastVal', hasUpdated', hasError') <-
    updateRefs (lastVal, hasUd, hasErr, fired, newVal)
  
  let newLoc' = replaceAtIndex i1 (lastVal'!!0) (latestVal cloc)
      newLoc'' = replaceAtIndex i2 (lastVal'!!1) newLoc'

  return CS { origVal = origVal cloc
            , latestVal = newLoc'' 
            , hasUpdated = hasUpdated'
            , hasError = hasError'
            }


varUpdates :: BoolVarMap -> (Transition, Ref) ->
  L BoolVarMap
varUpdates bvm (t, fired) =
 foldM (updateToLava fired) bvm (updates t)


isBlocked :: EventMap -> Automaton -> [Ref] -> L Ref
isBlocked evm a enabled =
 do
  let trans = transitions a
      
  enabledEvents <- sequence $
                    [ orl
                     [ enabledRef
                     | (t, enabledRef) <- zip trans enabled,
                       e == event t
                     ]
                    | e <- events a
                    ]

  let eventRefs = map (fromJust . flip lookup evm) (events a)
  blockedEvents <- sequence $ zipWith and2 eventRefs (map neg enabledEvents)
  
  orl blockedEvents



checkMarked :: PartialSynchCircuit -> (Automaton -> L Ref)
checkMarked psc a =
 do    
    predicatesHold <- sequence $ map (checkPredicate psc (autName a))
                                     (marked a)
    orl predicatesHold


checkPredicate :: PartialSynchCircuit -> Name -> (Predicate -> L Ref)
checkPredicate psc nm (loc, gs) =
 do
    let locRefs = latestVal $ fromJust $ lookup nm (locMap psc)
        rightLoc = locRefs!!loc
        bvs = boolVarMap psc
        bvrm = zip (map fst bvs) (map (latestVal . snd) bvs)
    gs <- sequence $ map (guardToLava bvrm) gs
    andl gs
    
guardToLava :: (BoolVarRefMap) -> Guard -> L Ref
guardToLava bvrm g = case (gval g) of
                          (True) -> return $ ref
                          (False) -> return $ neg $ ref
 where
  ref = fromJust $ lookup (gvar g) (bvrm)


updateToLava :: Ref -> BoolVarMap -> Update -> L BoolVarMap
updateToLava shouldUpdate bvs ud =
 do
  let varName = uvar ud
      newVal = if (uval ud) then tt else ff
      boolVar = fromJust $ lookup varName bvs
      lastVal = latestVal boolVar
      hasUd = hasUpdated boolVar
      hasErr = hasError boolVar

  (lastVal', hasUpdated', hasError') <-
    updateRef (lastVal, hasUd, hasErr, shouldUpdate, newVal)
    
  let newBoolVarState = CS { origVal = origVal boolVar
                           , latestVal = lastVal' 
                           , hasUpdated = hasUpdated'
                           , hasError = hasError'
                           }

  return $ replaceAt (varName, newBoolVarState) bvs
  

replaceAt :: (Eq a) => (a,b) -> [(a, b)] -> [(a, b)]
replaceAt _ [] = []
replaceAt (key, new) ((key', old):ps)
 | key == key' = (key, new):ps
 | key /= key' = (key', old):(replaceAt (key, new) ps)
 
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls =
 a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- input: lastval, hasUpdated, hasError, shouldUpdate, newVal
-- Output: lastval', hasUpdated', hasError'
updateRefs :: ([Ref], Ref, Ref, Ref, [Ref]) -> L ([Ref], Ref, Ref)
updateRefs (lastVal, hasUpdated, hasError, shouldUpdate, newVal) =
 do
    clash <- and2 shouldUpdate hasUpdated
    hasError' <- or2 clash hasError
    hasUpdated' <- or2 shouldUpdate hasUpdated
    nextVal <- sequence $ map (mux shouldUpdate) (zip lastVal newVal)
    return (nextVal, hasUpdated', hasError')

updateRef :: (Ref, Ref, Ref, Ref, Ref) -> L (Ref, Ref, Ref)
updateRef (a, b, c, d, e) =
 do ([f],g,h) <- updateRefs([a],b,c,d,[e])
    return (f,g,h)





locationOH :: Automaton -> L OneHotFlop
locationOH a = oneHotFlops (0, nbrLocations a)

-- Input is (hot_value, #values)
oneHotFlops :: (Int, Int) -> L OneHotFlop
oneHotFlops (val, max)
-- TODO: add case (maybe flag val=-1) for an all-maybe flop? Or would that
-- almost always come up as an illegal one-hot array?
  | 0 <= val && val < max =
    let (L m0) = sequence [ flop (Just (i==val))
                          | i <- [0..(max-1)] ]
    in L (\n0 -> let (tups, n1, gs1) = m0 n0
                     (ins, outs)     = unzip tups
                     outApp          = (zipWith ($) outs)
                  in ((ins, sequence_ . outApp), n1, gs1))    
  | otherwise = error "oneHotFlops: index out of bounds"

constantOneHot :: (Int, Int) -> OneHot
constantOneHot (val, max)
  | 0 <= val && val < max =
    [ if (i==val) then tt else ff | i <- [1..max] ]
  | otherwise = error "oneHotFlops: index out of bounds"

