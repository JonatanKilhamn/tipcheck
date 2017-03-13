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
import GHC.Enum

-- for testing only
import TestAut3
s :: String
s = "f0"
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
 | head s == '~' = findInputEvent (tail s) sc
 | otherwise = Just ""
--}

-- Takes a latch as described in tip output, and returns the
-- location it references
findLocation :: String -> SynchCircuit -> Maybe (Name, Location)
findLocation s sc
 | head s == 'R' = undefined
 | head s == '~' = findLocation (tail s) sc
 | otherwise = Just undefined


findLocInAut :: String -> (IndexedOneHot Location) -> Maybe Location
findLocInAut s ls = invLookup (Pos s) ls


-- Utilities:
invLookup :: Eq a => a -> [(b,a)] -> Maybe b
invLookup = flip $ (flip lookup) . (map swap)

combine :: ([a],[a]) -> [a]
combine = uncurry (++)




-- Data types

data OutputRef
  = I Int
  | R Int
  | NOut OutputRef
 deriving ( Eq )

instance Show OutputRef where
  show (I n) = "I" ++ (show n)
  show (R n) = "R" ++ (show n)
  show (NOut (NOut x)) = show x
  show (NOut x) = "~" ++ (show x)

-- These two functions ignore polarity
toRef :: OutputRef -> Ref
toRef (NOut x) = toRef x
toRef x = Pos (show x)
toName :: OutputRef -> Name
toName (NOut x) = toName x
toName x = show x



nego :: OutputRef -> OutputRef
nego (NOut x)  = x
nego x  = NOut x

getNbr :: OutputRef -> Int
getNbr (I n) = n
getNbr (R n) = n
getNbr (NOut x) = getNbr x

isInputGate :: OutputRef -> Bool
isInputGate (I _) = True
isInputGate (NOut x) = isInputGate x
isInputGate _ = False





---- reading the input

outputFile :: FilePath
outputFile = "output.txt"

--{--
readOutputFile :: IO [[OutputRef]]
readOutputFile =
 do
  fileContents <- readFile outputFile
  let fileLines = map (splitOn ",") . lines $ fileContents
      rawRefs = map (map readGate) fileLines
      relevant = filter (any isInputGate) rawRefs
      outputRefs = map formatOutputLine relevant
      --formatted = map (map (map formatOutputChar)) relevant
      --separated = map separateInputAndLatches formatted
      --nbrs = map maxGateNo formatted
      
      --transposed = 
  return $ outputRefs
--}



readGate :: String -> OutputRef
readGate [] = error "invalid gate in output (empty)"
readGate (s:ss)
 | s == 'i' = I (read ss :: Int)
 | s == 'f' = R (read ss :: Int)
 | s == '~' = (NOut . readGate) ss
 | otherwise = error $ "invalid gate in output: "++(s:ss)

formatOutputLine :: [OutputRef] -> [OutputRef]
formatOutputLine = combine . adjustLatchNbrs . separateInputAndLatches 

separateInputAndLatches :: [OutputRef] -> ([OutputRef],[OutputRef])
separateInputAndLatches ss = partition isInputGate ss

adjustLatchNbrs :: ([OutputRef],[OutputRef]) -> ([OutputRef],[OutputRef])
adjustLatchNbrs (is,ls) = (is,adjustGates (maxGateNo is) ls)
 where
  adjustGates n ss = map (adjustGate n) ss
  adjustGate n (R m) = R (n+m+1)
  adjustGate n (NOut x) = adjustGate n x
  adjustGate n x = x

maxGateNo :: [OutputRef] -> Int
maxGateNo = foldl (flip (max . getNbr)) (-1)





outputRefToGuard :: OutputRef -> SynchCircuit -> Guard
outputRefToGuard = undefined


outputVarMap :: SynchCircuit -> OutputRef -> (VarName, Int)
outputVarMap = undefined

outputLocMap :: SynchCircuit -> OutputRef -> (Automaton, Location)
outputLocMap = undefined

{--getLookup :: SynchCircuit -> OutputRef -> Maybe SCLookupEntry
getLookup sc r
 | isInputGate r = fmap SCLInp $ lookup ref inputs
 | otherwise = lookup r latches
 where
  ref = toRef r
  inputs = map swap (eventRefs sc)
  latches = locs ++ vars
  locs = createLocsEntries sc
  vars = undefined --createVarsEntries (length locs) sc
  --}

createLocsEntries :: SynchCircuit -> [(OutputRef, SCLookupEntry)]
createLocsEntries sc =
 zip (map R [1..])
     [ SCLLoc name loc
     | (name, loc) <- undefined]


type SCLookup = OutputRef -> SCLookupEntry
  
data SCLookupEntry
  = SCLInp Event
  | SCLVar VarName Int
  | SCLLoc Name Location
   deriving ( Show )



getLookup :: L SynchCircuit -> OutputRef -> Maybe SCLookupEntry
getLookup m r = lookup rname list
 where
  rname = toName r
  (sc, gs) = run m
  list = inputs ++ flops
  inputEntries = map (SCLInp . fst) (eventRefs sc)
  inputRefs  = [ n | (n, Input) <- gs ]
  inputs = zip inputRefs inputEntries
  flopRefs   = [ n | (n, Flop init x) <- gs ]
  locEntries = [SCLLoc aut loc
               | (aut, loc) <-
                  concat [ zip (repeat aut) (map fst locsToAut)
                         | (aut, locsToAut) <- locRefs sc
                         ]
               ]
  varEntries = [SCLVar vname val
               | (vname, val, _) <-
                 concat [ zip3 (repeat vname) [(offset+1)..] vrefs
                        | (vname, (vrefs, offset)) <- varRefs sc
                        ]
               ]
  flopEntries = locEntries ++ varEntries
  flops = zip flopRefs flopEntries


