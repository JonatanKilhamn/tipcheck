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

type IntVariable = ([Ref], Int)



refAt :: IntVariable -> Int -> Ref
(refs,offset) `refAt` i =
 let below = i <= offset
     above = i > offset + length refs
 in case (below,above) of
         (True,_) -> tt
         (_, True) -> ff
         (_,_) -> refs !! (i - offset - 1)

isValidIV :: IntVariable -> Bool
isValidIV un@(refs,offset) =
 and [ okay (refAt un i) (refAt un (i+1))
     | i <- range un ]
  where okay a b = case (a,b) of
                        (Bool False, Bool False) -> True
                        (Bool False, _) -> False
                        (_, _) -> True

range :: IntVariable -> [Int]
range (rs, o) = [o..(o + length rs)]

--type UnaryVariable = (Un, Variable)

type Flop a = (a, a -> L ())

type UnaryFlop = Flop IntVariable

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
type CVariable = CState IntVariable
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

type VarRefMap = [(VarName, IntVariable)]

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
       (IntConst i) -> compareIVConstant pred un i
       (IntVar y) -> compareIntVariables pred un (fromJust $ lookup y vrm)
       (Plus e1 e2) -> undefined --case (intExprToUn
       --TODO: recursion to handle plus and minus


-- TODO
unaryPlus :: IntVariable -> IntVariable -> L IntVariable
unaryPlus = undefined

unaryMinus :: IntVariable -> IntVariable -> L IntVariable
unaryMinus = undefined



compareIVConstant :: BinaryPred -> IntVariable -> Int -> L Ref
compareIVConstant pred un n =
 case (pred) of
      (Equals) -> and2 (un `refAt` n) (neg (un `refAt` (n+1)))
      (LessThan) -> return $ neg $ un `refAt` n
      (GreaterThanEq) -> fmap neg (compareIVConstant LessThan un n)
      (LessThanEq) -> compareIVConstant LessThan un (n+1)
      (GreaterThan) -> compareIVConstant GreaterThanEq un (n+1)
      


compareIntVariables :: BinaryPred -> IntVariable -> IntVariable -> L Ref
compareIntVariables pred unv1 unv2@([],o) = compareIVConstant pred unv1 o
compareIntVariables pred unv1@(un1,offs1) unv2@(un2,offs2) =
 do
  let (pairFun, diff) = funs pred
      unv2' = (un2, offs2+diff)
  parts <- sequence $ [ pairFun (unv1 `refAt` i) (unv2' `refAt` i)
                      | i <- union (range unv1) (range unv2') ]
  andl (tt:parts)
 where funs Equals = (eq2, 0)
       funs LessThanEq = (impl2, 0)
       funs LessThan = (impl2, -1)
       funs GreaterThanEq = (flip impl2, 0)
       funs GreaterThan = (flip impl2, 1)

updateToLava :: Ref -> VarMap -> Update -> L VarMap
updateToLava shouldUpdate vm (AssignInt varName expr) = 
 do
  let var = fromJust $ lookup varName vm
      lastVal = latestVal var
      hasUd = hasUpdated var
      hasErr = hasError var      
      
  newVal <- intExprToIntVar vm expr
    
  (lastVal', hasUpdated', hasError') <-
    updateIntVar (lastVal, hasUd, hasErr, shouldUpdate, newVal)

  let newVarState = CS { origVal = origVal var
                       , latestVal = lastVal'
                       , hasUpdated = hasUpdated'
                       , hasError = hasError'
                       }

  return $ replaceAt (varName, newVarState) vm


intExprToIntVar :: VarMap -> IntExpr -> L IntVariable
intExprToIntVar vm (IntConst n) = return ([],n)
intExprToIntVar vm (IntVar x) = return . latestVal . fromJust . (lookup x) $ vm
intExprToIntVar vm (Plus e1 e2) =
 do
  (refs1, offs1) <- intExprToIntVar vm e1
  (refs2, offs2) <- intExprToIntVar vm e2
  refs <- if (refs2 == []) then return refs1 else mergesortl (refs1++refs2)
  return (refs, offs1+offs2)
intExprToIntVar vm (Minus e1 (IntConst n)) =
 do
  (refs1, offs1) <- intExprToIntVar vm e1
  return (refs1, offs1-n)
intExprToIntVar vm (Minus e1 e2) =
 do
  (refs1, offs1) <- intExprToIntVar vm e1
  (refs2, offs2) <- intExprToIntVar vm e2
  --refs <- mergesortl (refs1++refs2)
  -- TODO: minus in the general case...
  return undefined



constIV :: Int -> Int -> IntVariable
constIV n max = ( [ if (i<n) then tt else ff | i <- [0 .. (max-1)] ]
                  , 0)
  
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


updateIntVar :: (IntVariable, Ref, Ref, Ref, IntVariable) ->
 L (IntVariable, Ref, Ref)
updateIntVar (lastVal, hasUpdated, hasError, shouldUpdate, newVal) =
 do let newRefs = [ newVal `refAt` i | i <- range lastVal ]
        (oldRefs, offset) = lastVal
    (nextRefs, hasUpdated', hasError') <-
      updateRefs (oldRefs, hasUpdated, hasError, shouldUpdate, newRefs)
    let overFlow = newVal `refAt` ((last $ range lastVal) + 1)
        underFlow = neg $ newVal `refAt` ((head $ range lastVal) - 1)
    hasError'' <- orl [overFlow, underFlow, hasError']
    return ((nextRefs, offset), hasUpdated', hasError'')


varFlop :: Variable -> L UnaryFlop
varFlop v =
 let (L m0) = sequence [ flop (Just (i <= (initial v)))
                       | i <- [((lower v)+1) .. (upper v)] ]
 in L (\n0 -> let (tups, n1, gs1) = m0 n0
                  (ins, outs)     = unzip tups
                  outApp          = (zipWith ($) outs)
              in ( ( (ins, lower v)
                   , sequence_ . outApp . fst) -- TODO: what happens when the output var has a different offset?
                 , n1, gs1 )
      )    


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

