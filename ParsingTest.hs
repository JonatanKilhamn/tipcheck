module ParsingTest where

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




--------------------------------------------------------------------------------
-- Circuits




test_prop :: L SynchCircuit -> L Props
test_prop lsc =
  do -- the circuit
     sc <- lsc
     
     -- never two phils holding the same fork
     --let currentY = fromJust $ lookup ("y") (varRefs sc)
     --b1 <- compareIVConstant Equals currentY 0
     --b2 <- compareIVConstant Equals currentY 5
     --b <- or2 b1 b2
     
     --bf <- finitely (neg b1)
     
     let uc = anyUncontr sc
          
     let err = anyError sc
     

     -- props
     return $ props
       { always = [neg err]
       , nevers  = [] --map snd $ boolVarRefs sc
       , finites = []--[[bf]]
       }
   

--auto_c :: Circuit
--auto_c = circuit auto_prop   
   
main :: IO ()
main =
 do s <- readWmodFile "Examples/odd_guard2.wmod"
    writeCircuit "Examples/auto" $ circuit $ test_prop $ processSystem s

circ s = circuit $ test_prop $ processSystem s
   
--------------------------------------------------------------------------------
-- Step example




oneHotBool :: (Int, Int) -> [Bool]
oneHotBool (val, max) = [ if (i == val) then True else False | i <- [1..max] ]

-- Output: last_constrs, bads, Circuit
steps :: Circuit -> [[Bool]] -> (Bool,[Bool],Circuit)
steps circ inputs = foldl foldableSteps (False,[],circ) inputs
 where
  size = length $ flops circ
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

