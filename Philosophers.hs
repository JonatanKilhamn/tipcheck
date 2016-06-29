module Philosophers where

import TransitionSystem
import TransitionSystemCircuits
import Lava

-- Dining philosophers system

nbrPhilosophers :: Int
nbrPhilosophers = 2

philList :: [Automaton]
philList = map philosopher [(i, nbrPhilosophers)
                           | i <- [0..nbrPhilosophers-1]]

philSynch = foldl synchronise emptySynch philList


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
