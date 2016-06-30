module Philosophers where

import TransitionSystem
import TransitionSystemCircuits
import Lava
import Data.Maybe

-- Dining philosophers



--------------------------------------------------------------------------------
-- Defining the automata
--

philList :: Int -> [Automaton]
philList nbr = map philosopher [(i, nbr)
                               | i <- [0..nbr-1]]

philSynch :: Int -> Synchronisation
philSynch = (foldr synchronise emptySynch . philList)


philosopher :: (Int,Int) -> Automaton
philosopher (p, max) = Aut { autName = "p"++this
                           , nbrLocations = 2
                           , transitions = ts
                           , marked = []
                           } 
 where
  this = show p
  left = show $ (p-1) `mod` max
  right = show $ (p+1) `mod` max
  ts = [ takeLeft
       , takeRight
       , eat
       , putDown
       ]
  takeLeft = Trans { start = 0
                   , event = "tl"++this
                   , guards = takeLeftGuards
                   , updates = [Update ("hl"++this) True]
                   , end = 0
                   }
  takeRight = Trans { start = 0
                    , event = "tr"++this
                    , guards = takeRightGuards
                    , updates = [Update ("hr"++this) True]
                    , end = 0
                    }
  eat = Trans { start = 0
              , event = "eat"++this
              , guards = eatGuards
              , updates = []
              , end = 1
              }
  putDown = Trans { start = 1
                  , event = "pd"++this
                  , guards = []
                  , updates = putDownUpdates
                  , end = 0
                  }
  takeLeftGuards = [ Guard ("hl"++this) False
                   , Guard ("hr"++left) False]
  takeRightGuards = [ Guard ("hr"++this) False
                    , Guard ("hl"++right) False]
  eatGuards = [ Guard ("hl"++this) True
              , Guard ("hr"++this) True]
  putDownUpdates = [ Update ("hl"++this) False
                   , Update ("hr"++this) False]



--------------------------------------------------------------------------------
-- Circuits



philsC :: Int -> L SynchCircuit
philsC n = processSystem (philSynch n) []

philsC_prop :: Int -> L Props
philsC_prop n =
  do -- the circuit
     sc <- philsC n
     
     -- never two phils holding the same fork
     held_twice <- sequence
                   [ and2 (holdingLeft sc p) (heldByLeft sc p)
                   | p <- [0..n-1]
                   ]
     b1 <- orl held_twice
     
     -- props
     return $ props
       { nevers  = [b1]
       , finites = []
       }
 where
   this p = show $ p `mod` n
   left p = show $ (p-1) `mod` n
   right p = show $ (p+1) `mod` n
   holdingLeft sc p = fromJust $ lookup ("hl"++this p) (boolVarRefs sc)
   heldByLeft sc p = fromJust $ lookup ("hr"++left p) (boolVarRefs sc)
