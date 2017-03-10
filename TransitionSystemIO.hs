module TransitionSystemIO where

import Data.Maybe
import Data.Function
import Data.List
import Data.List.Split
import Data.Tuple
import Data.Char
import Control.Monad
import Circuit
import Lava
import TransitionSystem
import TransitionSystemCircuits

-- for testing only
import TestAut3
s :: String
s = "I0"
sc :: SynchCircuit
(sc,_) = run testCirc


maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

hasRefName :: Ref -> Name -> Bool
hasRefName (Pos n1) n2 = n1==n2
hasRefName (Neg n1) n2 = n1==n2
hasRefName _ _ = False

getRefName :: Ref -> Maybe Name
getRefName (Pos n1) = Just n1
getRefName (Neg n1) = Just n1
getRefName _ = Nothing
--{--
findInputEvent :: String -> SynchCircuit -> Maybe Event
findInputEvent s sc
-- | head s == 'I' = fmap fst $ find (((flip hasRefName) s) . snd) (eventRefs sc)
 | head s == 'I' = invLookup (Pos s) (eventRefs sc)
 | head s == '~' = findInputEvent (drop 1 s) sc
 | otherwise = Just ""
--}

-- Takes a latch as described in tip output, and returns the
-- location it references
findLocation :: String -> SynchCircuit -> Maybe (Name, Location)
findLocation s sc
 | head s == 'R' = undefined
 | head s == '~' = findLocation (drop 1 s) sc
 | otherwise = Just undefined


findLocInAut :: String -> (IndexedOneHot Location) -> Maybe Location
findLocInAut s ls = invLookup (Pos s) ls

invLookup :: Eq a => a -> [(b,a)] -> Maybe b
invLookup = flip $ (flip lookup) . fmap swap









---- reading the input

outputFile :: FilePath
outputFile = "output.txt"

--{--
readOutputFile :: IO [[String]]
readOutputFile =
 do
  fileContents <- readFile outputFile
  let fileLines = map (splitOn ",") . lines $ fileContents
      relevant = (filter $ (any (=='i')) . concat) fileLines
  return $ map (map (map formatOutputChar)) relevant
--}


separateInputAndLatches :: [String] -> ([String],[String])
separateInputAndLatches ss = ( filter (any (=='i')) ss
                             , filter (not . (any (=='i'))) ss)


formatOutputChar :: Char -> Char
formatOutputChar 'i' = 'I'
formatOutputChar 'f' = 'R'
formatOutputChar x = x




















