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


-- possible values of nbrPhils: 5, 10
nbrPhils :: Int
nbrPhils = 5

-- possible values of nbrSteps: 10, 50, 100, 200, 500, 1000
nbrSteps :: Int
nbrSteps = 100

fileNameI :: Int -> Int -> FilePath
fileNameI i j = "Examples/HVC2017/EDP" ++ (show i) ++ "_"++ (show j) ++ ".wmod"

fileName :: FilePath
fileName = fileNameI nbrPhils nbrSteps

philSynch :: IO Synchronisation
philSynch = readWmodFile fileName

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

     let bad = uncontrBlock sc
     
     let err = anyError sc
     
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
  writeCircuit ("Examples/phils"++ (show nbrPhils) ++ "_"++ (show nbrSteps)) circ
  return circ

