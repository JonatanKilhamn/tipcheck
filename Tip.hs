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

inputFailedFile :: FilePath
inputFailedFile = "input_failed.aig"

prop_TipNothingStrange circ =
  Q.monadicIO $
    do res <- Q.run (tip circ [])
       Q.monitor (whenFail (writeCircuit inputFailedFile circ))
       Q.assert (exit res == ExitSuccess)
       Q.assert (length (safes res) == length (bads circ))
       Q.assert (length (lives res) == length (justs circ))

prop_TipSafe  = mkProp_TipWith True  ["-td=-1"]
prop_TipLive  = mkProp_TipWith True  ["-rip-bmc=2"]
prop_TipBmc   = mkProp_TipWith False ["-alg=bmc", "-k=100"]

prop_Niklas b (Mod (Yes,Yes,No,No) circ) =
  not (null (bads circ) && null (justs circ)) ==>
    Q.monadicIO $
      do res <- Q.run (tip circ (if b then [] else ["-td=-1"]))
         Q.monitor (whenFail (writeCircuit inputFailedFile circ))
         Q.assert (exit res == ExitSuccess)

         -- check all safety properties
         sequence_
           [ do res' <- Q.run (tip circ{ bads = [bads circ !! p], justs = [] }
                          (["-alg=bmc", "-k=100"] ++ (if b then ["-td=-1"] else [])))
                Q.assert (exit res' == ExitSuccess)
                Q.assert (not complete || not (null proved))
                Q.assert (null proved || safes res' == [(0,False) | not (head proved)])
           | p <- [0..length (bads circ)-1]
           , let proved = [ proved | (p',proved) <- safes res, p == p' ]
           ]
 where
  complete = True

mkProp_TipWith complete args circ =
  not (null (bads circ) && null (justs circ)) ==>
    Q.monadicIO $
      do res <- Q.run (tip circ args)
         Q.monitor (whenFail (writeCircuit inputFailedFile circ))
         Q.assert (exit res == ExitSuccess)

         -- check all safety properties
         sequence_
           [ do res' <- Q.run (tip circ{ bads = [bads circ !! p], justs = [] } ["-alg=bmc", "-k=100", "-td=-1"])
                Q.assert (exit res' == ExitSuccess)
                Q.assert (not complete || not (null proved))
                Q.assert (null proved || safes res' == [(0,False) | not (head proved)])
           | p <- [0..length (bads circ)-1]
           , let proved = [ proved | (p',proved) <- safes res, p == p' ]
           ]

         -- check all liveness properties
         sequence_
           [ do res' <- Q.run (tip circ{ justs = [justs circ !! p], bads = [] } ["-alg=bmc", "-k=100", "-td=-1"])
                Q.assert (exit res' == ExitSuccess)
                Q.assert (not complete || not (null proved))
                Q.assert (null proved || lives res' == [(0,False) | not (head proved)])
           | p <- [0..length (justs circ)-1]
           , let proved = [ proved | (p',proved) <- lives res, p == p' ]
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
 deriving ( Eq, Show )

tip :: Circuit -> [String] -> IO TipResult
tip circ args =
  do tryMany (writeCircuit inputFile circ)
     ret <- system (unwords ([ "" -- "perl -e 'alarm shift @ARGV; exec @ARGV' 4" -- timeout X seconds
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

