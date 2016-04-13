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
inputFailedFile  = "input_failed.aig"
inputFailedFile0 = "input_failed0.aig"
inputFailedFile2 = "input_failed2.aig"

prop_TipNothingStrange circ =
  Q.monadicIO $
    do res <- Q.run (tip circ [])
       Q.monitor (whenFail (writeCircuit inputFailedFile circ))
       Q.assert (exit res == ExitSuccess)
       Q.assert (length (safes res) == length (bads circ))
       Q.assert (length (lives res) == length (justs circ))

prop_TipSafe  = mkProp_TipWith True  ["-td=-1"]
prop_TipLive  = mkProp_TipWith True  ["-sc=1"]
prop_TipBmc   = mkProp_TipWith False ["-alg=bmc", "-k=100"]

prop_Liveness (Mod (Yes,No,Yes,Yes) circ) =
  not (null (justs circ)) ==>
    Q.monadicIO $
      do Q.monitor (whenFail (writeCircuit inputFailedFile circ))
         
         res <- Q.run (tip circ ["-sce=1"])
         Q.assert (exit res == ExitSuccess)

         sequence_
           [ do res' <- Q.run (tip circ{ justs = [justs circ !! p], bads = [] } ["-alg=bmc", "-k=100", "-td=-1"])
                Q.assert (exit res' == ExitSuccess)
                Q.assert (lives res' == exp)
           | p <- [0..length (justs circ)-1]
           , let exp = case [ proved | (p',proved) <- lives res, p == p' ] of
                         True : _ -> []
                         _        -> [(0,False)]
           ]

prop_TemporalDecomposition (Mod (No,Yes,No,No) circ) =
  not (null (bads circ) && null (justs circ)) ==>
    Q.monadicIO $
      do Q.monitor (whenFail (writeCircuit inputFailedFile circ))
         --Q.monitor (whenFail (writeCircuit inputFailedFile2 (double circ)))
         
         res1 <- Q.run (tip circ [])
         Q.assert (exit res1 == ExitSuccess)

         res2 <- Q.run (tip circ ["-td=-1"])
         Q.assert (exit res2 == ExitSuccess)
         Q.assert (res1 == res2)

prop_Doubling (Mod (No,Yes,No,No) circ) =
  not (null (bads circ) && null (justs circ)) ==>
    Q.monadicIO $
      do Q.monitor (whenFail (writeCircuit inputFailedFile circ))
         --Q.monitor (whenFail (writeCircuit inputFailedFile2 (double (double circ))))
         
         res1 <- Q.run (tip circ [])
         Q.assert (exit res1 == ExitSuccess)

         --res2 <- Q.run (tip (double "?" (double "!" circ)) [])
         res2 <- Q.run (tip (double "!" circ) [])
         Q.assert (exit res2 == ExitSuccess)
         Q.assert (res1 == res2)

prop_Step (Mod (No,Yes,No,No) circ) =
  not (null (bads circ) && null (justs circ)) ==>
    Q.monadicIO $
      do Q.monitor (whenFail (writeCircuit inputFailedFile circ))
         --Q.monitor (whenFail (writeCircuit inputFailedFile2 (double (double circ))))
         
         res1 <- Q.run (tip circ [])
         Q.assert (exit res1 == ExitSuccess)

         st0  <- Q.pick (vector (length (flops circ)))
         inps <- Q.pick (vector (length (inputs circ)))
         let (c, bs, circ') = step circ st0 inps

         res2 <- Q.run (tip circ' [])
         Q.assert (exit res2 == ExitSuccess)
         
         Q.monitor $ whenFail $ putStr $ unlines $
           [ show res1
           , show res2
           , show bs
           ]
         
         Q.assert ( length (safes res1) == length bs
                 && length (safes res2) == length bs
                 && and [ i==j && (not a || (not b0&&b))
                        | (((i,a),(j,b)),b0) <- (safes res1 `zip` safes res2) `zip` bs
                        ]
                  )

prop_DoublingBase (Mod (No,Yes,No,No) circ) =
  not (null (bads circ) && null (justs circ)) ==>
    Q.monadicIO $
      do Q.monitor (whenFail (writeCircuit inputFailedFile circ))
         Q.monitor (whenFail (writeCircuit inputFailedFile0 (time0 circ)))
         
         res1 <- Q.run (tip circ [])
         Q.assert (exit res1 == ExitSuccess)

         res2 <- Q.run (tip (time0 circ) [])
         Q.assert (exit res2 == ExitSuccess)
         Q.assert (length (safes res1) == length (safes res2))
         Q.assert $ and [ i==j && (not a || b) | ((i,a),(j,b)) <- safes res1 `zip` safes res2 ]

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
     ret <- system (unwords ([ "" -- "perl -e 'alarm shift @ARGV; exec @ARGV' 5" -- timeout X seconds
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
{-
  inputFile  = "/dev/shm/input.aig"
  resultFile = "/dev/shm/result.txt"
  outputFile = "/dev/shm/output.txt"
  errorFile  = "/dev/shm/error.txt"
-}

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

