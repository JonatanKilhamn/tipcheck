module PMEParsed where

import TransitionSystem
import TransitionSystemCircuits
import WmodParser
import Lava
import Data.Maybe
import Circuit
import Control.Monad
import qualified Data.Set as S

-- Parallel manufacturing example





--------------------------------------------------------------------------------
-- Defining the automata
--


fileName :: FilePath
fileName = "Examples/FMACAD2017/PME.wmod"

pmeSynch :: IO Synchronisation
pmeSynch = readWmodFile fileName

--------------------------------------------------------------------------------
-- Circuits


pme_sc :: IO (L SynchCircuit)
pme_sc = 
 do
  ps <- pmeSynch
  return $ processSystem ps


pme_prop :: L SynchCircuit -> L Props
pme_prop l_sc =
 do -- the circuit
     sc <- l_sc
     
     let bad = uncontrBlock sc
     
     let err = anyError sc
     
     -- props
     return $ props
       { always = [neg err]
       , nevers  = [anyContr sc, bad] {-- FOR NOW: FIRST 'never' MUST ALWAYS BE "ANY TRANSITIONS CONTROLLABLE" --}
       , finites = []
       }
   

pme_c :: L SynchCircuit -> Circuit
pme_c = circuit . pme_prop   
   
main :: IO Circuit
main = 
 do
  sc <- pme_sc
  let circ = pme_c sc
  writeCircuit ("examples/pme") circ
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

