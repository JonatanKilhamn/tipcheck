module TestAut where

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
autSynch = (foldr synchronise emptySynch [testAutB, testAutA])

testAutA :: Automaton
testAutA = Aut { autName = "Aut1"
               , locations = S.fromList [locA, locB]
               , transitions = ts
               , marked = []
               , initialLocation = locA
               } 
 where
  ts = [ downA
       , upA
       ]
  downA = Trans { start = locA
                , event = "a"
                , guards = []
                , updates = []
                , end = locB
                , uncontrollable = True
                }
  upA = Trans { start = locB
              , event = "b"
              , guards = []
              , updates = []
              , end = locA
              , uncontrollable = False
              }
  locA = "A1"
  locB = "B1"
  

testAutB :: Automaton
testAutB = Aut { autName = "Aut2"
               , locations = S.fromList [locA, locB]
               , transitions = ts
               , marked = []
               , initialLocation = locA
               } 
 where
  ts = [ downB
       , upB
       ]
  downB = Trans { start = locA
                , event = "b"
                , guards = []
                , updates = []
                , end = locB
                , uncontrollable = False
                }
  upB = Trans { start = locB
              , event = "c"
              , guards = []
              , updates = []
              , end = locA
              , uncontrollable = False
              }
  locA = "A2"
  locB = "B2"



--------------------------------------------------------------------------------
-- Circuits



testCirc :: L SynchCircuit
testCirc = processSystem autSynch

test_prop :: L Props
test_prop =
  do -- the circuit
     sc <- testCirc

     let err = anyError sc
     
     bad <- and2 (atLoc sc "Aut1" "B1") (atLoc sc "Aut2" "B2")

     -- props
     return $ props
       { always = [neg err]
       , nevers  = [neg $ anyUncontr sc, bad] {-- FOR NOW: FIRST 'never' MUST ALWAYS BE "ALL TRANSITIONS CONTROLLABLE" (i.e. the negation of "any transition uncontrollable". --}
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

