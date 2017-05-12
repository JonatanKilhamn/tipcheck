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

-- (1,5) (3,3) (5,5) (7,7)

nbrFloors :: Int
nbrFloors = 1

nbrCats :: Int
nbrCats = 5

fileNameI :: Int -> FilePath
fileNameI i = "Examples/FMCAD2017/CMT" ++ (show i) ++ "_"++ (show i) ++ ".wmod"

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
  
       --"Examples/HVC2014/EDP5_10.wmod"
       --"Examples/simple_selfloop.wmod"
       --"Examples/cat_mouse.wmod"


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
     
     -- each philosopher gets to eat infinitely often
     -- TODO
     
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
   ("examples/cmt"++ (show nbrFloors) ++ "_" ++ (show nbrCats)) circ
  return circ

--------------------------------------------------------------------------------
-- Step example


-- Output: last_constrs, bads, Circuit
{--stepscmt :: Int -> [[Bool]] -> (Bool,[Bool],Circuit)
stepscmt n inputs = foldl foldableSteps (False,[],circ) inputs
 where
  circ = cmt_c n
  size = length $ flops circ
  foldableSteps (_,_,c) ins = step c (none size) ins
--}

none :: Int -> [Bool]
none = flip replicate False

{--tl1, tr1, eat1, pd1, tl0, tr0, eat0, pd0 :: [Bool]
tl1 = eventInput "tl1" (cmtynch testNbr)
tr1 = eventInput "tr1" (cmtynch testNbr)
eat1 = eventInput "eat1" (cmtynch testNbr)
pd1 = eventInput "pd1" (cmtynch testNbr)
tl0 = eventInput "tl0" (cmtynch testNbr)
tr0 = eventInput "tr0" (cmtynch testNbr)
eat0 = eventInput "eat0" (cmtynch testNbr)
pd0 = eventInput "pd0" (cmtynch testNbr)
--}

fstpair3 :: (a,b,c) -> (a,b)
fstpair3 (a,b,c) = (a,b)

--step c (replicate 8 False) (replicate 8 False)

