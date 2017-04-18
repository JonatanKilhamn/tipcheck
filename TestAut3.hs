module TestAut3 where

import TransitionSystem
import TransitionSystemCircuits
import Lava
import Data.Maybe
import Data.List
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

xlimit :: Int
xlimit = 3

autSynch :: Synchronisation
autSynch = setVars vars sys
 where sys = (foldr synchronise emptySynch [testAut])
       vars = [ ("x", mkVar (0,xlimit+1,0))
              , ("y", mkVar (0,2,0))]

testAut :: Automaton
testAut = Aut { autName = "Aut1"
              , locations = S.fromList [loc0, loc1, loc2, loc3, loc4, loc5]
              , transitions = ts
              , marked = []
              , initialLocation = loc0
              , uncontrollable = ["alpha"]
              } 
 where
  ts = [ t02
       , t01
       , t23
       , t13
       , t31
       , t35
       , t34
       , t44
       ]
  t02 = Trans { start = loc0
              , event = "a"
              , guards = []
              , updates = [AssignInt ("y") (IntConst 2)]
              , end = loc2
              }
  t01 = Trans { start = loc0
              , event = "b"
              , guards = []
              , updates = [AssignInt ("y") (IntConst 1)]
              , end = loc1
              }
  t23 = Trans { start = loc2
              , event = "b"
              , guards = []
              , updates = []
              , end = loc3
              }
  t13 = Trans { start = loc1
              , event = "a"
              , guards = []
              , updates = []
              , end = loc3
              }
  t31 = Trans { start = loc3
              , event = "c"
              , guards = []
              , updates = [AssignInt ("x") (Plus (IntVar "x") (IntConst 1))]
              , end = loc1
              }
  t35 = Trans { start = loc3
              , event = "alpha"
              , guards = [ GInt Equals ("y") (IntConst 2)
                         , GInt GreaterThan ("x") (IntConst xlimit)]
              , updates = []
              , end = loc5
              }
  t34 = Trans { start = loc3
              , event = "alpha"
              , guards = [ GInt Equals ("y") (IntConst 2)
                         , GInt LessThanEq ("x") (IntConst xlimit)]
              , updates = []
              , end = loc4
              }
  t44 = Trans { start = loc4
              , event = "omega"
              , guards = []
              , updates = []
              , end = loc4
              }
  loc0 = "l0"
  loc1 = "l1"
  loc2 = "l2"
  loc3 = "l3"
  loc4 = "l4"
  loc5 = "l5"
  


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
     let bad = atLoc sc "Aut1" "l5"

     -- props
     return $ props
       { always = [neg err]
       , nevers  = [anyContr sc, bad] {-- FOR NOW: FIRST 'never' MUST ALWAYS BE "ANY TRANSITIONS CONTROLLABLE" --}
       , finites = []
       }
 where
   atLoc sc name loc = fromJust $ lookup loc $ fromJust $ lookup (name) (locRefs sc)
   --bCounter sc = fst . fromJust $ lookup ("bcounter") (varRefs sc)

test_c :: Circuit
test_c = circuit test_prop
   
main :: IO ()
main = writeCircuit "examples/test3" test_c

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

a, b, c, alpha, omega :: [Bool]
a = eventInput "a" autSynch
b = eventInput "b" autSynch
c = eventInput "c" autSynch
alpha = eventInput "alpha" autSynch
omega = eventInput "omega" autSynch
{--a = oneHotBool (1,3)
b = oneHotBool (2,3)
c = oneHotBool (3,3)--}

fstpair3 :: (a,b,c) -> (a,b)
fstpair3 (a,b,c) = (a,b)

--step c (replicate 8 False) (replicate 8 False)

