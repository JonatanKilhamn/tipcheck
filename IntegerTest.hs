module IntegerTest where

import TransitionSystem
import TransitionSystemCircuits
import WmodParser
import Lava
import Data.Maybe
import Circuit
import Control.Monad
import qualified Data.Set as S

-- An automaton used for testing the implementation of integer-valued variables



--------------------------------------------------------------------------------
-- Defining the automata
--


synch :: Synchronisation
synch = setDefault ("x",0) $
        setRangeMin ("x",0) $
        setRangeMax ("x",10) $
        setDefault ("y",0) $
        setRangeMin ("y",0) $
        setRangeMax ("y",5) $
        (synchronise automaton) emptySynch


loc1, loc2 :: Location
loc1 = "loc1"
loc2 = "loc2"

automaton :: Automaton
automaton = Aut { autName = "auto1"
                , locations = S.fromList [loc1, loc2]
                , transitions = ts
                , marked = []
                , initialLocation = loc1
                } 
 where
  ts = [ loop
       , moveOn
       , secondLoop
       ]
  loop = Trans { start = loc1
               , event = "loop"
               , guards = [GInt LessThan ("x") (IntConst 10)]
               , updates = incrX
               , end = loc1
               , uncontrollable = False
               }
  moveOn = Trans { start = loc1
                 , event = "moveOn"
                 , guards = [GInt Equals ("x") (IntConst 10)]
                 , updates = incrY5
                 , end = loc2
                 , uncontrollable = True
                 }
  secondLoop = Trans { start = loc2
                     , event = "secondLoop"
                     , guards = []
                     , updates = []
                     , end = loc2
                     , uncontrollable = False
                     }
  incrX = [AssignInt ("x") (Plus (IntVar "x") (IntConst 1))]
  incrY5 = [AssignInt ("y") (Minus (IntVar "x") (IntConst 8))]


--------------------------------------------------------------------------------
-- Circuits



synchCircuit :: L SynchCircuit
synchCircuit = processSystem synch

auto_prop :: L Props
auto_prop =
  do -- the circuit
     sc <- synchCircuit
     
     -- never two phils holding the same fork
     let currentY = fromJust $ lookup ("y") (varRefs sc)
     b1 <- compareIVConstant Equals currentY 0
     b2 <- compareIVConstant Equals currentY 5
     b <- or2 b1 b2
     
     --bf <- finitely (neg b1)
     
     let uc = anyUncontr sc
          
     let err = anyError sc
     

     -- props
     return $ props
       { always = [neg err]
       , nevers  = [] --map snd $ boolVarRefs sc
       , finites = []--[[bf]]
       }
   

auto_c :: Circuit
auto_c = circuit auto_prop   
   
main :: IO ()
main = writeCircuit "Examples/auto" auto_c
   
--------------------------------------------------------------------------------
-- Step example




oneHotBool :: (Int, Int) -> [Bool]
oneHotBool (val, max) = [ if (i == val) then True else False | i <- [1..max] ]

-- Output: last_constrs, bads, Circuit
steps :: [[Bool]] -> (Bool,[Bool],Circuit)
steps inputs = foldl foldableSteps (False,[],auto_c) inputs
 where
  size = length $ flops auto_c
  foldableSteps (_,_,c) ins = step c (none size) ins




none :: Int -> [Bool]
none = flip replicate False

loop, moveOn, secondLoop :: [Bool]
{-tl0 = oneHotBool (1,4)
tr0 = oneHotBool (2,4)
eat0 = oneHotBool (3,4)
pd0 = oneHotBool (4,4)-}
loop = oneHotBool (1,3)
moveOn = oneHotBool (2,3)
secondLoop = oneHotBool (3,3)

fstpair3 :: (a,b,c) -> (a,b)
fstpair3 (a,b,c) = (a,b)

--step c (replicate 8 False) (replicate 8 False)

