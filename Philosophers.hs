module Philosophers where

import TransitionSystem
import TransitionSystemCircuits
import Lava
import Data.Maybe
import Circuit
import Control.Monad
import qualified Data.Set as S

-- Dining philosophers





--------------------------------------------------------------------------------
-- Defining the automata
--

philList :: Int -> [Automaton]
philList nbr = map philosopher [ (i, nbr, 2)
                               | i <- [0..nbr-1] ]

forkList :: Int -> [Automaton]
forkList nbr = map fork [ (i,nbr)
                        | i <- [0..nbr-1] ]

philSynch :: Int -> Synchronisation
philSynch nbr = foldr synchronise emptySynch ((philList nbr) ++ (forkList nbr))

thinking, eating, ready, lu :: Location
thinking = "thinking"
eating = "eating"
ready = "ready"
lu = "lu"

intermediateLoc :: Int -> Location
intermediateLoc nbr = ("intermediate" ++ show nbr)

philosopher :: (Int,Int,Int) -> Automaton
philosopher (p, max, inter) = Aut { autName = "p"++this
                                  , locations = locs
                                  , transitions = ts
                                  , marked = [(thinking, [])]
                                  , initialLocation = thinking
                                  , uncontrollable =
                                    if even p
                                    then [event takeLeft]
                                    else []
                                  } 
 where
  locs = S.fromList $ [thinking, ready, eating, lu] ++
                      (map intermediateLoc [1..inter])
  this = show p
  right = show $ (p+1) `mod` max
  ts = [ takeLeft
       , takeRight
       , eat
       , putDown] ++
       (map intermediate [1..inter])
  takeLeft = Trans { start = thinking
                   , event = "take"++this++"_"++this
                   , guards = []
                   , updates = []
                   , end = lu
                   --, uncontrollable = even p -- even numbered phils are uncontr.
                   }
  takeRight = Trans { start = intermediateLoc inter
                    , event = "take"++this++"_"++right
                    , guards = []
                    , updates = []
                    , end = ready
                    --, uncontrollable = False
                    }
  eat = Trans { start = ready
              , event = "eat"++this
              , guards = []
              , updates = []
              , end = eating
              --, uncontrollable = False
              }
  putDown = Trans { start = eating
                  , event = "put"++this
                  , guards = []
                  , updates = []
                  , end = thinking
                  --, uncontrollable = False
                  }
  intermediate n = Trans { start = case n of
                                        1 -> lu
                                        _ -> intermediateLoc (n-1)
                         , event = "intermediate"++this
                         , guards = []
                         , updates = []
                         , end = intermediateLoc n
                         --, uncontrollable = False
                         }


onTable, inHand :: Location
onTable = "onTable"
inHand = "inHand"

fork :: (Int,Int) -> Automaton
fork (f, max) = Aut { autName = "f"++this
                    , locations = S.fromList [onTable, inHand]
                    , transitions = ts
                    , marked = [(onTable, [])]
                    , initialLocation = onTable
                    , uncontrollable = []
                    } 
 where
  this = show f
  left = show $ (f-1) `mod` max
  ts = [ leftTakes
       , rightTakes
       , leftPuts
       , rightPuts]
  leftTakes = Trans { start = onTable
                    , event = "take"++left++"_"++this
                    , guards = []
                    , updates = []
                    , end = inHand
                    --, uncontrollable = False
                    }
  rightTakes = Trans { start = onTable
                     , event = "take"++this++"_"++this
                     , guards = []
                     , updates = []
                     , end = inHand
                     --, uncontrollable = False
                     }
  leftPuts = Trans { start = inHand
                   , event = "put"++left
                   , guards = []
                   , updates = []
                   , end = onTable
                   --, uncontrollable = False
                   }
  rightPuts = Trans { start = inHand
                    , event = "put"++this
                    , guards = []
                    , updates = []
                    , end = onTable
                    --, uncontrollable = False
                    }
--------------------------------------------------------------------------------
-- Circuits

testNbr :: Int
testNbr = 10


phils :: Int -> L SynchCircuit
phils n = processSystem (philSynch n)

testCirc :: L SynchCircuit
testCirc = phils testNbr


phils_prop :: Int -> L Props
phils_prop n =
  do -- the circuit
     sc <- phils n
     
     -- never two phils holding the same fork
     {--held_twice <- sequence
                   [ and2 ((holdingLeft sc p)!!0) ((heldByLeft sc p)!!0)
                   | p <- [0..n-1]
                   ]
     b1 <- orl held_twice--}
     
     -- phil_0 never holding both eir forks:
     --b2 <- and2 ((holdingLeft sc 0)!!0) ((heldByLeft sc 1)!!0)
     
     
     --let b3 = isEating sc 0
     
     
     
     let err = anyError sc
     
     --bad <- and2 b1 (neg err)
     
     -- each philosopher gets to eat infinitely often
     -- TODO
     
     -- props
     return $ props
       { always = [neg err]
       , nevers  = [anyContr sc] {-- FOR NOW: FIRST 'never' MUST ALWAYS BE "ANY TRANSITIONS CONTROLLABLE" --}
       --, nevers  = [neg uc, b1] --held_twice --map snd $ boolVarRefs sc
       , finites = []
       }
 where
   this p = show $ p `mod` n
   left p = show $ (p-1) `mod` n
   right p = show $ (p+1) `mod` n
   --holdingLeft sc p = fst . fromJust $ lookup ("hl"++this p) (varRefs sc)
   --heldByLeft sc p = fst . fromJust $ lookup ("hr"++left p) (varRefs sc)
   --isEating sc p = fromJust $ lookup "eating" $ fromJust $ lookup ("p"++this p) (locRefs sc)
   

phils_c :: Int -> Circuit
phils_c = circuit . phils_prop   
   
main :: IO ()
main = writeCircuit "examples/phils2" (phils_c testNbr)

write :: Int -> IO ()
write n = writeCircuit "examples/phils2" (phils_c n)

--------------------------------------------------------------------------------
-- Step example

autSynch = philSynch testNbr


-- Output: last_constrs, bads, Circuit
stepsPhils :: Int -> [[Bool]] -> (Bool,[Bool],Circuit)
stepsPhils n inputs = foldl foldableSteps (False,[],circ) inputs
 where
  circ = phils_c n
  size = length $ flops circ
  foldableSteps (_,_,c) ins = step c (none size) ins


none :: Int -> [Bool]
none = flip replicate False

tl1, tr1, eat1, pd1, tl0, tr0, eat0, pd0 :: [Bool]
tl1 = eventInput "tl1" (philSynch testNbr)
tr1 = eventInput "tr1" (philSynch testNbr)
eat1 = eventInput "eat1" (philSynch testNbr)
pd1 = eventInput "pd1" (philSynch testNbr)
tl0 = eventInput "tl0" (philSynch testNbr)
tr0 = eventInput "tr0" (philSynch testNbr)
eat0 = eventInput "eat0" (philSynch testNbr)
pd0 = eventInput "pd0" (philSynch testNbr)

fstpair3 :: (a,b,c) -> (a,b)
fstpair3 (a,b,c) = (a,b)

--step c (replicate 8 False) (replicate 8 False)

