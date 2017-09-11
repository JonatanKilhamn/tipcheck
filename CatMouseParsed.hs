module CatMouseParsed where

import TransitionSystem
import TransitionSystemCircuits
import WmodParser
import Lava
import Data.Maybe
import Circuit
import Control.Monad
import qualified Data.Set as S

-- Cat and Mouse towers





--------------------------------------------------------------------------------
-- Defining the automata
--

-- Previously used values are (1,1) (1,5) (3,3) (5,5) (7,7)
-- Possible values of nbrFloors are 1,3,5,7
-- nbrCats should be able to take any value > 0

nbrFloors :: Int
nbrFloors = 1

nbrCats :: Int
nbrCats = 5

fileNameI :: Int -> FilePath
fileNameI i = "Examples/HVC2017/CMT" ++ (show i) ++ "_"++ (show i) ++ ".wmod"

fileName :: FilePath
fileName = fileNameI nbrFloors


cmtSynch :: IO Synchronisation
cmtSynch = 
 do
  synch <- readWmodFile fileName
  let varList = concat $ [ [ ("c_"++(show floor)++(show room)
                             , Variable { lower = 0
                                        , upper = nbrCats
                                        , initial = if (floor==1)&&(room==1)
                                                    then nbrCats
                                                    else 0
                                        }
                             )
                           ] ++
                           [ ("m_"++(show floor)++(show room)
                             , Variable { lower = 0
                                        , upper = nbrCats
                                        , initial = if (floor==nbrFloors)&&(room==5)
                                                    then nbrCats
                                                    else 0
                                        }
                             )
                           ]
                         | floor <- [1..nbrFloors]
                         , room <- [1..5]
                         ]    
      synch2 = setVars varList synch
  return synch2 { automata = map changeAut $ automata synch2 }
 where changeAut aut = aut { transitions = map changeTrans $ transitions aut }
       changeTrans trans = trans { guards = map changeGuard $ guards trans }
       changeGuard (GInt LessThan vn ie) = GInt LessThan vn (IntConst nbrCats)
       changeGuard g = g
  

--------------------------------------------------------------------------------
-- Circuits


cmt_sc :: IO (L SynchCircuit)
cmt_sc = 
 do
  ps <- cmtSynch
  return $ processSystem ps


cmt_prop :: L SynchCircuit -> L Props
cmt_prop l_sc =
 do -- the circuit
     sc <- l_sc
     
     bad <- orl (failureStates sc)
     
     let err = anyError sc
          
     -- props
     return $ props
       { always = [neg err]
       , nevers  = [anyContr sc, bad] {-- FOR NOW: FIRST 'never' MUST ALWAYS BE "ANY TRANSITIONS CONTROLLABLE" --}
       , finites = []
       }
 where
   failureStates sc = catMaybes $ map (lookup "F") $ map snd (locRefs sc)
   

cmt_c :: L SynchCircuit -> Circuit
cmt_c = circuit . cmt_prop   
   
main :: IO Circuit
main = 
 do
  sc <- cmt_sc
  let circ = cmt_c sc
  writeCircuit --("examples/cmt"++ (show nbrFloors) ++ "_x") circ
   ("Examples/cmt"++ (show nbrFloors) ++ "_" ++ (show nbrCats)) circ
  return circ


