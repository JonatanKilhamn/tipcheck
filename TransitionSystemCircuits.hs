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


data CSynch
  = SynchC
  { locMap :: LocationsMap
  , boolVarMap :: BoolVarMap
  , eventMap   :: EventMap
  , globalError :: Ref
  }


processSystem :: Synchronisation -> [Ref] -> L [Ref]
processSystem s ins =
   do 
     let evm = [(x, Pos x) | x <- allEvents s]
     
      -- create state variables
     varFlops <- sequence [ flop Nothing | var <- allBoolVars s ]
     
     -- create location state variables
     locFlops <- sequence [ locationOH aut | aut <- automata s ]
     let locRefs = map fst locFlops
     
     -- create big clumsy object to pass around
     let auts = [ (name, (newState loc))
                | ((loc, _), name) <- zip locFlops (map autName $ automata s) ]
         bvs = zip (allBoolVars s) (map (newState . fst) varFlops)
         state = SynchC { locMap = auts
                        , boolVarMap = bvs
                        , eventMap   = evm
                        , globalError = ff
                        }
     
     -- process each transition
     let contextTrans = [ (t, aName)
                        | a <- automata s,
                          t <- transitions a,
                          let aName = autName a
                        ]
     state1 <- foldM processTransition state contextTrans
     
     -- set the updated location values
     sequence_ $ zipWith ($)
       (map snd locFlops)
       (map (latestVal . snd) (locMap state1))
       
     -- set the updated variable values
     sequence_ $ zipWith ($)
       (map snd varFlops)
       (map (latestVal . snd) (boolVarMap state1))     
     
     -- compute errors
     locErr <- orl (map (hasError . snd) (locMap state1))
     varErr <- orl (map (hasError . snd) (boolVarMap state1))
     localError <- or2 locErr varErr 
     finalError <- or2 localError (globalError state1)
     
     return $ head $ (map (latestVal . snd) (locMap state1))



processTransition :: CSynch -> (Transition, Name) -> L CSynch
processTransition cs (t, an) =
 do
  let auts = locMap cs
      bvs = boolVarMap cs
      evm = eventMap cs
      eventRef = fromJust $ lookup (event t) evm
      cloc = fromJust $ lookup an auts
      i1 = start t
      i2 = end t
      startLocRef = (origVal cloc) !! i1
   
  -- check whether the event is fired
  transFired <- and2 eventRef (startLocRef)
          
  -- check all the guards
  gs <- sequence $ map (guardToLava bvs) (guards t)
  clearedGuards <- andl gs
  blocked <- and2 transFired (neg clearedGuards)
  
  newBvs <- foldM (updateToLava transFired) bvs (updates t)
  
  -- update locations
  -- input: lastval, hasUpdated, hasError, shouldUpdate, newVal
  let lastVal = map ((latestVal cloc) !!) [i1, i2]
      hasUd = hasUpdated cloc
      hasErr = hasError cloc
      newVal = [ff, tt]
  (lastVal', hasUpdated', hasError') <-
    updateRefs (lastVal, hasUd, hasErr, transFired, newVal)
  

  
  let newLoc' = replaceAtIndex i1 (lastVal'!!0) (latestVal cloc)
      newLoc'' = replaceAtIndex i2 (lastVal'!!1) newLoc'


  let newLocationState = CS { origVal = origVal cloc
                            , latestVal = newLoc'' 
                            , hasUpdated = hasUpdated'
                            , hasError = hasError'
                            }
      auts' = replaceAt (an, newLocationState) auts
  
  return SynchC { locMap = auts'
                , boolVarMap = newBvs
                , eventMap   = evm
                , globalError = blocked
                }




guardToLava :: (BoolVarMap) -> Guard -> L Ref
guardToLava bvm g = case (gval g) of
                         (True) -> return $ ref
                         (False) -> return $ neg $ ref
 where
  ref = origVal $ fromJust $ lookup (gvar g) (bvm)





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

