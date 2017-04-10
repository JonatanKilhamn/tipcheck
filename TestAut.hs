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
       , loopA
       ]
  downA = Trans { start = locA
                , event = "a"
                , guards = []
                , updates = [AssignInt ("acounter") (IntConst 0)]
                , end = locB
                , uncontrollable = False
                }
  upA = Trans { start = locB
              , event = "b"
              , guards = []
              , updates = [AssignInt ("acounter") (IntConst 0)]
              , end = locA
              , uncontrollable = False
              }
  loopA = Trans { start = locB
                , event = "c"
                , guards = [GInt Equals ("bcounter") (IntConst 1)]
                , updates = [AssignInt ("acounter") (IntConst 1)]
                , end = locB
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
                , updates = [AssignInt ("bcounter") (IntConst 1)]
                , end = locB
                , uncontrollable = True
                }
  upB = Trans { start = locB
              , event = "c"
              , guards = []
              , updates = [AssignInt ("bcounter") (IntConst 0)]
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
     
     --bad <- and2 (atLoc sc "Aut1" "B1") (atLoc sc "Aut2" "B2")
     let bad = (bCounter sc)!!0

     -- props
     return $ props
       { always = [neg err]
       , nevers  = [anyContr sc, bad] {-- FOR NOW: FIRST 'never' MUST ALWAYS BE "ANY TRANSITIONS CONTROLLABLE" --}
       , finites = []
       }
 where
   atLoc sc name loc = fromJust $ lookup loc $ fromJust $ lookup (name) (locRefs sc)
   bCounter sc = fst . fromJust $ lookup ("bcounter") (varRefs sc)

test_c :: Circuit
test_c = circuit test_prop   
   
main :: IO ()
main = writeCircuit "examples/test1" test_c

--------------------------------------------------------------------------------
-- Step example





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
a = eventInput "a" autSynch
b = eventInput "b" autSynch
c = eventInput "c" autSynch
{--a = oneHotBool (1,3)
b = oneHotBool (2,3)
c = oneHotBool (3,3)--}

fstpair3 :: (a,b,c) -> (a,b)
fstpair3 (a,b,c) = (a,b)

--step c (replicate 8 False) (replicate 8 False)

