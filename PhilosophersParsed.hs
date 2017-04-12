module PhilosophersParsed where

import TransitionSystem
import TransitionSystemCircuits
import WmodParser
import Lava
import Data.Maybe
import Circuit
import Control.Monad
import qualified Data.Set as S

-- Dining philosophers





--------------------------------------------------------------------------------
-- Defining the automata
--

-- 10, 50, 100, 200, 500, 1000

nbrPhils :: Int
nbrPhils = 5

nbrSteps :: Int
nbrSteps = 1000

fileNameI :: Int -> Int -> FilePath
fileNameI i j = "Examples/HVC2014/EDP" ++ (show i) ++ "_"++ (show j) ++ ".wmod"

fileName :: FilePath
fileName = fileNameI nbrPhils nbrSteps

philSynch :: IO Synchronisation
philSynch = readWmodFile fileName
       --"Examples/HVC2014/EDP5_10.wmod"
       --"Examples/simple_selfloop.wmod"
       --"Examples/cat_mouse.wmod"


--------------------------------------------------------------------------------
-- Circuits


phils_sc :: IO (L SynchCircuit)
phils_sc = 
 do
  ps <- philSynch
  return $ processSystem ps


phils_prop :: L SynchCircuit -> L Props
phils_prop l_sc =
 do -- the circuit
     sc <- l_sc
     
     let evenPhils = filter even [1..nbrPhils]
     
     uncontr_blocks <- sequence
      [ and2 (isHeld sc p) (isThinking sc p)
      | p <- evenPhils
      ]

     bad <- orl uncontr_blocks
     
     let err = anyError sc
     
     --bad <- and2 b1 (neg err)
     
     -- each philosopher gets to eat infinitely often
     -- TODO
     
     -- props
     return $ props
       { always = [neg err]
       , nevers  = [anyContr sc, bad] {-- FOR NOW: FIRST 'never' MUST ALWAYS BE "ANY TRANSITIONS CONTROLLABLE" --}
       , finites = []
       }
 where
   philAut p = "Philo:"++(show p)
   forkAut f = "Fork:"++(show f)
   isHeld sc f = fromJust $ lookup "1" $ fromJust $ lookup (forkAut f) (locRefs sc)
   isThinking sc p = fromJust $ lookup "think" $ fromJust $ lookup (philAut p) (locRefs sc)
   

phils_c :: L SynchCircuit -> Circuit
phils_c = circuit . phils_prop   
   
main :: IO Circuit
main = 
 do
  sc <- phils_sc
  let circ = phils_c sc
  writeCircuit ("examples/phils"++ (show nbrPhils) ++ "_"++ (show nbrSteps)) circ
  return circ

--------------------------------------------------------------------------------
-- Step example


-- Output: last_constrs, bads, Circuit
{--stepsPhils :: Int -> [[Bool]] -> (Bool,[Bool],Circuit)
stepsPhils n inputs = foldl foldableSteps (False,[],circ) inputs
 where
  circ = phils_c n
  size = length $ flops circ
  foldableSteps (_,_,c) ins = step c (none size) ins
--}

none :: Int -> [Bool]
none = flip replicate False

{--tl1, tr1, eat1, pd1, tl0, tr0, eat0, pd0 :: [Bool]
tl1 = eventInput "tl1" (philSynch testNbr)
tr1 = eventInput "tr1" (philSynch testNbr)
eat1 = eventInput "eat1" (philSynch testNbr)
pd1 = eventInput "pd1" (philSynch testNbr)
tl0 = eventInput "tl0" (philSynch testNbr)
tr0 = eventInput "tr0" (philSynch testNbr)
eat0 = eventInput "eat0" (philSynch testNbr)
pd0 = eventInput "pd0" (philSynch testNbr)
--}

fstpair3 :: (a,b,c) -> (a,b)
fstpair3 (a,b,c) = (a,b)

--step c (replicate 8 False) (replicate 8 False)

