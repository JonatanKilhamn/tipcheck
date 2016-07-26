module TransitionSystemCircuits where

import Data.Maybe
import Data.Function
import Data.List
import Control.Monad
import Circuit
import Lava
import TransitionSystem
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Exts




--------------------



type OneHot = [Ref]
type IndexedOneHot k = [(k, Ref)]
type Bin = [Ref]

type Un = [Ref]

--type UnaryVariable = (Un, Variable)

type Flop a = (a, a -> L ())

type UnaryFlop = Flop Un

type OneHotFlop = Flop OneHot --(OneHot, OneHot -> L ())
type IndexedOneHotFlop k = Flop (IndexedOneHot k)--(IndexedOneHot k, (IndexedOneHot k) -> L ())

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

type CLocation = CState (IndexedOneHot Location)
type CVariable = CState Un
type CEvent = Ref


type LocationsMap = [(Name, CLocation)]

type VarMap = [(VarName, CVariable)]

type EventMap = [(Event, CEvent)]

data PartialSynchCircuit
  = PSC
  { locMap :: LocationsMap
  , varMap :: VarMap
  , eventMap   :: EventMap
  , globalError :: Ref
  , uncontr :: Ref
  }

type LocationRefMap = [(Name, IndexedOneHot Location)]

type VarRefMap = [(VarName, Un)]

type EventRefMap = [(Event, Ref)]

type MarkedRefMap = [(Name, Ref)]

data SynchCircuit
  = SynchC
  { locRefs :: LocationRefMap
  , varRefs :: VarRefMap
  , eventRefs   :: EventRefMap
  , markedRefs :: MarkedRefMap
  , anyError :: Ref
  , anyUncontr :: Ref
  }
 deriving ( Show )


processSystem :: Synchronisation -> L SynchCircuit
processSystem s =
   do
     -- input processing
     evRefs <- sequence [ input | x <- allEvents s]

     let autNames = map autName (automata s)
         allVarNames = M.keys (allVars s)
         evm = zip (allEvents s) evRefs

     -- create location state variables
     locFlops <- sequence [ locationOH aut | aut <- automata s ]
     let locRefs = map fst locFlops

      -- create state variables
     varFlops <- sequence [ varFlop $ (allVars s) M.! var
                          | var <- allVarNames
                          ]
     
     -- create big clumsy object to pass around
     let auts = [ (name, (newState loc))
                | ((loc, _), name) <- zip locFlops autNames ]
         vs = zip allVarNames (map (newState . fst) varFlops)
         state = PSC { locMap = auts
                     , varMap = vs
                     , eventMap   = evm
                     , globalError = ff
                     , uncontr = ff
                     }
     
     -- process each automaton
     state1 <- foldM processAutomaton state (automata s)

     -- set the updated location values
     let newLocs = map (latestVal . snd) (locMap state1)
     sequence_ $ zipWith ($)
       (map snd locFlops) newLocs

     -- set the updated variable values
     let newVars = map (latestVal . snd) (varMap state1)
     sequence_ $ zipWith ($)
       (map snd varFlops) newVars
     
     -- compute which automata are in marked states
     markedRefs <- sequence $ map (checkMarked state1)
                                  (automata s)
     let marked = zip autNames markedRefs
     
     -- compute errors
     locErr <- orl (map (hasError . snd) (locMap state1))
     varErr <- orl (map (hasError . snd) (varMap state1))
     localError <- or2 locErr varErr
     
     validInput <- isOH evRefs
     error1 <- or2 localError (neg validInput)
     finalError <- or2 error1 (globalError state1)
     
     -- output
     let circuit = SynchC { locRefs = zip autNames newLocs
                          , varRefs = zip allVarNames newVars
                          , eventRefs = evm
                          , markedRefs = marked
                          , anyError = finalError
                          , anyUncontr = (uncontr state1)
                          }
     return circuit



processAutomaton :: PartialSynchCircuit -> Automaton ->
  L PartialSynchCircuit
processAutomaton state a =
 do
  let trans = transitions a
      vm = varMap state
      evm = eventMap state
      auts = locMap state
      an = autName a
      cloc = fromJust $ lookup an auts
      getEventRef t = fromJust $ lookup (event t) evm

  -- create refs signifying which transitions are enabled
  enableds <- sequence $ map (isEnabledTransition cloc vm) trans
  -- refs signifying which transitions are fired
  fireds <- sequence $ zipWith and2 (map getEventRef trans) enableds
  
  let transAndFireds = zip trans fireds
    
  -- update locations
  cloc' <- foldM locationUpdate cloc transAndFireds
  let auts' = replaceAt (an, cloc') auts
  
  -- update state variables
  vm' <- foldM varUpdates vm transAndFireds
  
  -- update controllability
  uncontrFound <- orl [ f | (t, f) <- transAndFireds, uncontrollable t ]
  uncontr' <- or2 uncontrFound (uncontr state)
      
  -- find errors stemming from a blocking automaton
  autError <- isBlocked evm a enableds
  globalError' <- or2 autError (globalError state)
  
  return state { locMap = auts'
               , varMap = vm'
               , globalError = globalError'
               , uncontr = uncontr'
               }

isEnabledTransition :: CLocation -> VarMap -> Transition ->
  L Ref
isEnabledTransition cloc vm t =
 do
  let startLocRef = fromJust $ lookup (start t) (origVal cloc)
  
  -- check all the guards
  let vrm = zip (map fst vm) (map (origVal . snd) vm)
  gs <- sequence $ map (guardToLava vrm) (guards t)
  clearedGuards <- andl gs
  and2 startLocRef clearedGuards

locationUpdate :: CLocation -> (Transition, Ref) -> L CLocation
locationUpdate cloc (t, fired) =
 do
  let l1 = start t
      l2 = end t
      startLocRef = (origVal cloc) `at` l1
  -- update locations
  -- input: lastval, hasUpdated, hasError, shouldUpdate, newVal
  let lastVal = map ((latestVal cloc) `at`) [l1, l2]
      hasUd = hasUpdated cloc
      hasErr = hasError cloc
      newVal = [ff, tt]
  (lastVal', hasUpdated', hasError') <-
    updateRefs (lastVal, hasUd, hasErr, fired, newVal)
  
  let newLoc' = replaceAt (l1, (lastVal' !! 0)) (latestVal cloc)
      newLoc'' = replaceAt (l2, (lastVal' !! 1)) newLoc'

  return CS { origVal = origVal cloc
            , latestVal = newLoc'' 
            , hasUpdated = hasUpdated'
            , hasError = hasError'
            }


varUpdates :: VarMap -> (Transition, Ref) ->
  L VarMap
varUpdates vm (t, fired) =
 foldM (updateToLava fired) vm (updates t)

isBlocked :: EventMap -> Automaton -> [Ref] -> L Ref
isBlocked evm a enabled =
 do
  let trans = transitions a
      
  enabledEvents <- sequence $
                    [ orl [ enabledRef
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
        rightLoc = locRefs `at` loc
        vm = varMap psc
        vrm = zip (map fst vm) (map (latestVal . snd) vm)
    gs <- sequence $ map (guardToLava vrm) gs
    andl gs

guardToLava :: VarRefMap -> Guard -> L Ref
guardToLava vrm (GInt pred x exp) =
 do 
  let un = fromJust $ lookup x vrm
  case (exp) of
       (IntConst i) -> compareUnaryConstant pred un i
       (IntVar y) -> compareUnaries pred un (fromJust $ lookup y vrm)
       --TODO: recursion to handle plus and minus

-- TODO
unaryPlus :: Un -> Un -> L Un
unaryPlus = undefined

unaryMinus :: Un -> Un -> L Un
unaryMinus = undefined


compareUnaryConstant :: BinaryPred -> Un -> Int -> L Ref
compareUnaryConstant pred un n
 = let above = (n > length un)
       belowEq = (n <= 0)
       exact = (n == length un)
   in
 case (pred) of
      (Equals) -> and2
                  (un !! (n-1)) -- TODO handle case n==0
                  (neg $ if exact then ff else (un !! n))
      (LessThan) -> return $
       case (above, belowEq) of
            (True, _) -> tt
            (_, True) -> ff
            (_)       -> neg $ un !! (n-1)
      (GreaterThanEq) -> return $
       case (above, belowEq) of
            (True, _) -> ff
            (_, True) -> tt
            (_)       -> un !! (n-1)
      (LessThanEq) -> compareUnaryConstant LessThan un (n+1)
      (GreaterThan) -> compareUnaryConstant GreaterThanEq un (n+1)
      
  

compareUnaries :: BinaryPred -> Un -> Un -> L Ref
compareUnaries pred un1 un2 =
 do
  let (xs, ys) = pad un1 un2 ff
      (pairFun, argFun) = funs pred
  parts <- sequence $ map (uncurry pairFun) (argFun xs ys)
  andl parts
 where funs Equals = (eq2, zip)
       funs LessThanEq = (impl2, zip)
       funs LessThan = (impl2, oneDiffZip ff)
       funs GreaterThanEq = (flip impl2, zip)
       funs GreaterThan = (flip impl2, flip (oneDiffZip ff))
       oneDiffZip e as bs = zip as ((tail bs) ++ [e])



pad :: [a] -> [a] -> a -> ([a],[a])
pad xs ys e = ( xs ++ (replicate (-n) e)
              , ys ++ (replicate (n) e))
 where n = (length xs) - (length ys)


updateToLava :: Ref -> VarMap -> Update -> L VarMap
updateToLava shouldUpdate vm (AssignInt varName expr) = 
 do
  let var = fromJust $ lookup varName vm
      lastVal = latestVal var
      hasUd = hasUpdated var
      hasErr = hasError var      
      
      newVal =
       case (expr) of
            (IntConst n) -> constUn n (length lastVal)
            (IntVar x) -> latestVal $ fromJust $ lookup x vm
    
  (lastVal', hasUpdated', hasError') <-
    updateRefs (lastVal, hasUd, hasErr, shouldUpdate, newVal)

  let newBoolVarState = CS { origVal = origVal var
                           , latestVal = lastVal' 
                           , hasUpdated = hasUpdated'
                           , hasError = hasError'
                           }

  return $ replaceAt (varName, newBoolVarState) vm


constUn :: Int -> Int -> Un
constUn n max = [ if (i<n) then tt else ff | i <- [0 .. (max-1)] ]
  
at :: Eq a => [(a,b)] -> a -> b
m `at` i = fromJust $ lookup i m

replaceAt :: (Eq a) => (a,b) -> [(a, b)] -> [(a, b)]
replaceAt _ [] = []
replaceAt (key, new) ((key', old):ps)
 | key == key' = (key, new):ps
 | key /= key' = (key', old):(replaceAt (key, new) ps)
 
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls =
 a ++ (item:b) where (a, (_:b)) = splitAt n ls


-- TODO: re-write this with some sort of typeclass!
-- For Un, single Refs and other unknown Ref-lists there will be no effect except maybe hides implementation. For OneHots, that thing we do with picking only the refs that need updating is moved here instead.

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




varFlop :: Variable -> L UnaryFlop
varFlop v =
 let (L m0) = sequence [ flop (Just (i <= (initial v)))
                       | i <- [((lower v)+1) .. (upper v)] ]
 in L (\n0 -> let (tups, n1, gs1) = m0 n0
                  (ins, outs)     = unzip tups
                  outApp          = (zipWith ($) outs)
                  outsUn          = outApp . fst
                  insUn           = (ins, v)
              in ((ins, sequence_ . outApp), n1, gs1))    


locationOH :: Automaton -> L (IndexedOneHotFlop Location)
locationOH a =
 do
   let locs = sort $ S.toList $ locations a
       init = fromJust $ elemIndex (initialLocation a) locs
   let (L m0) = oneHotFlops (init, length locs)
   L (\n0 -> let ((ins, f), n1, gs1) = m0 n0
                 sameOrder indexed   = map snd $ (sortWith fst indexed)
                 f'                  = f . sameOrder
              in ((zip locs ins, f'),n1, gs1))

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

