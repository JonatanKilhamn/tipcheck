module Philosophers where

import TransitionSystem
import TransitionSystemCircuits
import Lava
import Data.Maybe
import Circuit
import Control.Monad
import qualified Data.Set as S

-- Dining philosophers





--------------------------------------------------------------------------------
-- Defining the automata
--

philList :: Int -> [Automaton]
philList nbr = map philosopher [(i, nbr)
                               | i <- [0..nbr-1]]

philSynch :: Int -> Synchronisation
philSynch = (foldr synchronise emptySynch . philList)


idle, eating :: Location
idle = "idle"
eating = "eating"

philosopher :: (Int,Int) -> Automaton
philosopher (p, max) = Aut { autName = "p"++this
                           , locations = S.fromList [idle, eating]
                           , transitions = ts
                           , marked = []
                           , initialLocation = idle
                           } 
 where
  this = show p
  left = show $ (p-1) `mod` max
  right = show $ (p+1) `mod` max
  ts = [ takeLeft
       , takeRight
       , eat
       , putDown
       ]
  takeLeft = Trans { start = idle
                   , event = "tl"++this
                   , guards = takeLeftGuards
                   , updates = [AssignInt ("hl"++this) (IntConst 1)]
                   , end = idle
                   , uncontrollable = False
                   }
  takeRight = Trans { start = idle
                    , event = "tr"++this
                    , guards = takeRightGuards
                    , updates = [AssignInt ("hr"++this) (IntConst 1)]
                    , end = idle
                    , uncontrollable = False
                    }
  eat = Trans { start = idle
              , event = "eat"++this
              , guards = eatGuards
              , updates = []
              , end = eating
              , uncontrollable = True
              }
  putDown = Trans { start = eating
                  , event = "pd"++this
                  , guards = []
                  , updates = putDownUpdates
                  , end = idle
                  , uncontrollable = False
                  }
  takeLeftGuards = [ GInt Equals ("hl"++this) (IntConst 0)
                   , GInt Equals ("hr"++left) (IntConst 0)]
  takeRightGuards = [ GInt Equals ("hr"++this) (IntConst 0)
                    , GInt Equals ("hl"++right) (IntConst 0)]
  eatGuards = [ GInt Equals ("hl"++this) (IntConst 1)
              , GInt Equals ("hr"++this) (IntConst 1)]
  putDownUpdates = [ AssignInt ("hl"++this) (IntConst 0)
                   , AssignInt ("hr"++this) (IntConst 0)]


--------------------------------------------------------------------------------
-- Circuits



phils :: Int -> L SynchCircuit
phils n = processSystem (philSynch n)

phils_prop :: Int -> L Props
phils_prop n =
  do -- the circuit
     sc <- phils n
     
     -- never two phils holding the same fork
     held_twice <- sequence
                   [ and2 ((holdingLeft sc p)!!0) ((heldByLeft sc p)!!0)
                   | p <- [0..n-1]
                   ]
     b1 <- orl held_twice
     
     let uc = anyUncontr sc
     
     
     let err = anyError sc
     
     bad <- and2 b1 (neg err)
     
     -- each philosopher gets to eat infinitely often
     -- TODO
     
     -- props
     return $ props
       { always = [neg err]
       , nevers  = [b1] --map snd $ boolVarRefs sc
       , finites = []
       }
 where
   this p = show $ p `mod` n
   left p = show $ (p-1) `mod` n
   right p = show $ (p+1) `mod` n
   holdingLeft sc p = fromJust $ lookup ("hl"++this p) (varRefs sc)
   heldByLeft sc p = fromJust $ lookup ("hr"++left p) (varRefs sc)
   

phils_c :: Int -> Circuit
phils_c = circuit . phils_prop   
   
main :: IO ()
main = writeCircuit "Examples/phils2" (phils_c 2)
   
--------------------------------------------------------------------------------
-- Step example




oneHotBool :: (Int, Int) -> [Bool]
oneHotBool (val, max) = [ if (i == val) then True else False | i <- [1..max] ]

-- Output: last_constrs, bads, Circuit
stepsPhils :: Int -> [[Bool]] -> (Bool,[Bool],Circuit)
stepsPhils n inputs = foldl foldableSteps (False,[],phils_c n) inputs
 where
  size = length $ head inputs
  foldableSteps (_,_,c) ins = step c (none size) ins


none :: Int -> [Bool]
none = flip replicate False

tl1, tr1, eat1, pd1, tl0, tr0, eat0, pd0 :: [Bool]
{-tl0 = oneHotBool (1,4)
tr0 = oneHotBool (2,4)
eat0 = oneHotBool (3,4)
pd0 = oneHotBool (4,4)-}
tl1 = oneHotBool (1,8)
tr1 = oneHotBool (2,8)
eat1 = oneHotBool (3,8)
pd1 = oneHotBool (4,8)
tl0 = oneHotBool (5,8)
tr0 = oneHotBool (6,8)
eat0 = oneHotBool (7,8)
pd0 = oneHotBool (8,8)

fstpair3 :: (a,b,c) -> (a,b)
fstpair3 (a,b,c) = (a,b)

--step c (replicate 8 False) (replicate 8 False)

