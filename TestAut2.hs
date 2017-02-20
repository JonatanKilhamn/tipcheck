module TestAut2 where

import TransitionSystem
import TransitionSystemCircuits
import Lava
import Data.Maybe
import Circuit
import Control.Monad
import qualified Data.Set as S

-- Testing automata





--------------------------------------------------------------------------------
-- Defining the automata
--

{-
autList :: Int -> [Automaton]
autList nbr = map testAut [(i, nbr)
                          | i <- [0..nbr-1]]
-}

autSynch :: Synchronisation
autSynch = (foldr synchronise emptySynch [testAutA])

testAutA :: Automaton
testAutA = Aut { autName = "Aut1"
               , locations = S.fromList [locA, locB]--, locC, locD]
               , transitions = ts
               , marked = []
               , initialLocation = locA
               } 
 where
  ts = [ right1
       , loop1 ]{--
       , right2
       , right3
       ]-}
  right1 = Trans { start = locA
                 , event = "a"
                 , guards = []
                 , updates = []
                 , end = locB
                 , uncontrollable = True
                }
  loop1 = Trans { start = locA
                , event = "b"
                , guards = []
                , updates = []
                , end = locA
                , uncontrollable = True
                }                
  right2 = Trans { start = locB
                 , event = "b"
                 , guards = []
                 , updates = []
                 , end = locC
                 , uncontrollable = False
                }
  right3 = Trans { start = locC
                 , event = "c"
                 , guards = []
                 , updates = []
                 , end = locD
                 , uncontrollable = False
                }
  locA = "A"
  locB = "B"
  locC = "C"
  locD = "D"
  
  


--------------------------------------------------------------------------------
-- Circuits



testCirc :: L SynchCircuit
testCirc = processSystem autSynch

test_prop :: L Props
test_prop =
  do -- the circuit
     sc <- testCirc

     let err = anyError sc
     --err <- or2 (anyError sc) (neg (anyUncontr sc))
     
     let bad = (atLoc sc "Aut1" "B")--anyUncontr sc
     

     -- props
     return $ props
       { always = [neg err]
       , nevers  = [bad]
       , finites = []
       }
 where
   atLoc sc name loc = fromJust $ lookup loc $ fromJust $ lookup (name) (locRefs sc)

test_c :: Circuit
test_c = circuit test_prop   
   
main :: IO ()
main = writeCircuit "examples/test1" test_c

--------------------------------------------------------------------------------
-- Step example




oneHotBool :: (Int, Int) -> [Bool]
oneHotBool (val, max) = [ if (i == val) then True else False | i <- [1..max] ]


-- Output: last_constrs, bads, Circuit
stepsTest :: [[Bool]] -> (Bool,[Bool],Circuit)
stepsTest inputs = foldl foldableSteps (False,[],circ) inputs
 where
  circ = test_c
  size = length $ flops circ
  foldableSteps (_,_,c) ins = step c (none size) ins


none :: Int -> [Bool]
none = flip replicate False

a, b, c :: [Bool]
a = oneHotBool (1,3)
b = oneHotBool (2,3)
c = oneHotBool (3,3)

fstpair3 :: (a,b,c) -> (a,b)
fstpair3 (a,b,c) = (a,b)

--step c (replicate 8 False) (replicate 8 False)

