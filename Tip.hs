{-# LANGUAGE TemplateHaskell #-}
module Tip where

import Circuit

import Test.QuickCheck
import Test.QuickCheck.All
import qualified Test.QuickCheck.Monadic as Q
import Data.Char
import Control.Exception
import System

--------------------------------------------------------------------------------

inputFailedFile = "input_failed.aig"

prop_TipNothingStrange circ =
  Q.monadicIO $
    do res <- Q.run (tip circ [])
       Q.monitor (whenFail' (writeCircuit inputFailedFile circ))
       Q.assert (exit res == ExitSuccess)
       Q.assert (length (safes res) == length (bads circ))

prop_TipCombined circ =
  not (null (bads circ) && null (justs circ)) ==>
    Q.monadicIO $
      do res <- Q.run (tip circ [])
         Q.monitor (whenFail' (writeCircuit inputFailedFile circ))

         -- check all safety properties
         sequence_
           [ do res' <- Q.run (tip circ{ bads = [bads circ !! p] } ["-alg=bmc", "-k=100"])
                Q.assert (exit res' == ExitSuccess)
                Q.assert (safes res' == [(0,False) | not proved])
           | p <- [0..length (bads circ)-1]
           , let proved = head [ proved | (p',proved) <- safes res, p == p' ]
           ]

         -- check all liveness properties
         sequence_
           [ do res' <- Q.run (tip circ{ justs = [justs circ !! p] } ["-alg=bmc", "-k=100"])
                Q.assert (exit res' == ExitSuccess)
                Q.assert (lives res' == [(0,False) | not proved])
           | p <- [0..length (justs circ)-1]
           , let proved = head [ proved | (p',proved) <- lives res, p == p' ]
           ]

-- template Haskell magic
checkAll = $(quickCheckAll)

--------------------------------------------------------------------------------

data TipResult
  = TipResult
  { exit  :: ExitCode
  , safes :: [(Int,Bool)]
  , lives :: [(Int,Bool)]
  }

tip :: Circuit -> [String] -> IO TipResult
tip circ args =
  do tryMany (writeCircuit inputFile circ)
     ret <- system (unwords ([ "perl -e 'alarm shift @ARGV; exec @ARGV' 2" -- timeout X seconds (for now)
                             , tipExe
                             , inputFile
                             , resultFile ]
                             ++ args ++
                             [ ">",  outputFile
                             , "2>", errorFile
                             ]))
     s <- tryMany (readFileStrict resultFile)
     let results safs livs [] =
            TipResult
            { exit  = ret
            , safes = safs
            , lives = livs
            }
         
         results safs livs (r:('b':p@(_:_)):ls) | r `elem` ["0","1"] && all isDigit p =
           results (safs ++ [(read p, r == "0")]) livs ls
         
         results safs livs (r:('j':p@(_:_)):ls) | r `elem` ["0","1"] && all isDigit p =
           results safs (livs ++ [(read p, r == "0")]) ls
         
         results safs livs (_:ls) =
           results safs livs ls
     return (results [] [] (lines s))
 where
  tipExe = "../build/debug-fast/tip3/tip"

  inputFile  = "input.aig"
  resultFile = "result.txt"
  outputFile = "output.txt"
  errorFile  = "error.txt"

--------------------------------------------------------------------------------
-- helpers

tryMany :: IO a -> IO a
tryMany io =
  do mx <- try io
     case mx of
       Left e  -> tryMany io where _ = e :: IOException
       Right x -> return x

readFileStrict :: FilePath -> IO String
readFileStrict file =
  do s <- readFile file
     (s==s) `seq` return s

--------------------------------------------------------------------------------

