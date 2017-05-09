module PhilosophersOld where

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
philList nbr = map philosopher [(i, nbr)
                               | i <- [0..nbr-1]]

philSynch :: Int -> Synchronisation
philSynch = foldr synchronise emptySynch . philList

idle, eating :: Location
idle = "idle"
eating = "eating"

philosopher :: (Int,Int) -> Automaton
philosopher (p, max) = Aut { autName = "p"++this
                           , locations = S.fromList [idle, eating]
                           , transitions = ts
                           , marked = []
                           , initialLocation = idle
                           , uncontrollable = [event eat]
                           } 
 where
  this = show p
  left = show $ (p-1) `mod` max
  right = show $ (p+1) `mod` max
  ts = [ takeLeft
       , takeRight
       , eat]
       {--, putDown
       ]--}
  takeLeft = Trans { start = idle
                   , event = "tl"++this
                   , guards = takeLeftGuards
                   , updates = [AssignInt ("hl"++this) (IntConst 1)]
                   , end = idle
                   --, uncontrollable = False
                   }
  takeRight = Trans { start = idle
                    , event = "tr"++this
                    , guards = takeRightGuards
                    , updates = [AssignInt ("hr"++this) (IntConst 1)]
                    , end = idle
                    --, uncontrollable = False
                    }
  
  eat = Trans { start = idle
              , event = "eat"++this
              , guards = eatGuards
              , updates = []
              , end = eating
              --, uncontrollable = True
              }
  putDown = Trans { start = eating
                  , event = "pd"++this
                  , guards = []
                  , updates = putDownUpdates
                  , end = idle
                  --, uncontrollable = True
                  }
  takeLeftGuards = []--[ GInt Equals ("hl"++this) (IntConst 0)]
                   --, GInt Equals ("hr"++left) (IntConst 0)]
  takeRightGuards = []--[ GInt Equals ("hr"++this) (IntConst 0)]
                    --, GInt Equals ("hl"++right) (IntConst 0)]
  eatGuards = []--[ GInt Equals ("hl"++this) (IntConst 1)
              --, GInt Equals ("hr"++this) (IntConst 1)]
  putDownUpdates = []--[ AssignInt ("hl"++this) (IntConst 0)
                   --, AssignInt ("hr"++this) (IntConst 0)]


--------------------------------------------------------------------------------
-- Circuits

testNbr :: Int
testNbr = 70


phils :: Int -> L SynchCircuit
phils n = processSystem (philSynch n)

testCirc :: L SynchCircuit
testCirc = phils testNbr


phils_prop :: Int -> L Props
phils_prop n =
  do -- the circuit
     sc <- phils n
     
     -- never two phils holding the same fork
     held_twice <- sequence
                   [ and2 ((holdingLeft sc p)!!0) ((heldByLeft sc p)!!0)
                   | p <- [0..n-1]
                   ]
     b1 <- orl held_twice
     
     -- phil_0 never holding both eir forks:
     b2 <- and2 ((holdingLeft sc 0)!!0) ((heldByLeft sc 1)!!0)
     
     
     let b3 = isEating sc 0
          
     
     let err = anyError sc
     
     bad <- and2 b1 (neg err)
     
     -- each philosopher gets to eat infinitely often
     -- TODO
     
     -- props
     return $ props
       { always = [neg err]
       , nevers  = [anyContr sc, b1] {-- FOR NOW: FIRST 'never' MUST ALWAYS BE "ANY TRANSITIONS CONTROLLABLE" --}
       --, nevers  = [neg uc, b1] --held_twice --map snd $ boolVarRefs sc
       , finites = []
       }
 where
   this p = show $ p `mod` n
   left p = show $ (p-1) `mod` n
   right p = show $ (p+1) `mod` n
   holdingLeft sc p = fst . fromJust $ lookup ("hl"++this p) (varRefs sc)
   heldByLeft sc p = fst . fromJust $ lookup ("hr"++left p) (varRefs sc)
   isEating sc p = fromJust $ lookup "eating" $ fromJust $ lookup ("p"++this p) (locRefs sc)
   

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

