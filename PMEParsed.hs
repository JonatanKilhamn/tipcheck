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
fileName = "Examples/HVC2017/PME.wmod"

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

