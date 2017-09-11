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
import GHC.Exts
import Control.Monad.Writer


import PhilosophersParsed
import CatMouseParsed
import PMEParsed
import TestAut3
s :: String
s = "f0"
--sc :: SynchCircuit
--(sc,_) = run testCirc







{--maybeHead :: [a] -> Maybe a
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
--}

-- Utilities:
invLookup :: Eq a => a -> [(b,a)] -> Maybe b
invLookup = flip $ (flip lookup) . (map swap)

combine :: ([a],[a]) -> [a]
combine = uncurry (++)

isNeg :: Ref -> Bool
isNeg (Neg x) = True
isNeg _ = False


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

isNegative :: OutputRef -> Bool
isNegative (I _) = False
isNegative (R _) = False
isNegative (NOut x) = not (isNegative x)



main :: IO Synchronisation
main = do
  s <- philSynch -- EDP
  -- s <- cmtSynch -- CMT
  -- s <- pmeSynch -- PME
  updateSynch s outputFile


---- reading the input

outputFile :: FilePath
outputFile = "output.txt"


type Invariant = [OutputRef]

readOutputFile :: FilePath -> IO [Invariant]
readOutputFile outputFile =
 do
  fileContents <- readFile outputFile
  let fileLines = map (splitOn ",") . lines $ fileContents
      rawRefs = map (map readGate) fileLines
      relevant = filter (any isInputGate) rawRefs
      invariants = map formatOutputLine relevant
  return invariants

updateSynch :: Synchronisation -> FilePath -> IO Synchronisation
updateSynch synch fp =
 do
  invariants <- readOutputFile fp
  let m = processSystem synch
      strs = catMaybes $ map (invariantToGuards m) invariants
  return $ foldr applyStrengthening synch strs
      


data Strengthening
  = Strengthen
  { strLocs   :: [(Name,Location)]
  , strEvent   :: Event
  , strVarGuards  :: [Guard]
  , strLocGuards :: [(Name,Location,Guard)]--the aut and loc which the guard refers to, i.e. doesn't need to be there
  , newVars :: [VarName]
  }
 deriving ( Eq, Show )



applyStrengthening :: Strengthening -> Synchronisation -> Synchronisation
applyStrengthening str synch = setVars newVars $ synch { automata = newAut }
 where (newAut, newVars) = runWriter $ mapM (strengthenAutomaton str) (automata synch)
 


strengthenAutomaton :: Strengthening -> Automaton ->
  Writer [(VarName, Variable)] Automaton
strengthenAutomaton str aut =
 if (strEvent str) `elem` (uncontrollable aut)
 then error "Cannot strengthen uncontrollable event!"
 else tell newLocVars >> return withNewLocVars
 where starts = [locN | (autN,locN) <- (strLocs str), autN==thisN]
       thisN = autName aut
       locsToAddVarsFor = if (any ((/=thisN) . fst) (strLocs str))
                          then [ln | (a,ln)<- strLocs str, a==thisN]
                          else []
       createVariable locN = (newLocVarName thisN locN, newLocVar aut locN)
       newLocVars = map createVariable locsToAddVarsFor
       hasNewGuards t = (event t)==(strEvent str) && (start t) `elem` starts
       relevantLocGuards t = map (newLocGuard t) (strLocGuards str)
       newLocGuard t (autN, locN, g)
        | autN /= (autName aut) = g
        | locN == (start t) = if (denotesLocFalse g) then Bottom else Top
        | otherwise = if (denotesLocFalse g) then Top else Bottom
       denotesLocFalse (GInt NEquals _ _) = True
       denotesLocFalse _ = False
       disjunct t = disjunctionGuard ((strVarGuards str)++(relevantLocGuards t))
       newGuards t = if (disjunct t) == Top
                     then guards t
                     else (disjunct t):(guards t)
       addGuards t = if (hasNewGuards t)
                     then t { guards = newGuards t }
                     else t
       withNewGuards = aut { transitions = map addGuards (transitions aut) }
       withNewLocVars = foldr addLocVar withNewGuards locsToAddVarsFor 


addLocVar :: Location -> Automaton -> Automaton
addLocVar ln a =
 let vn = newLocVarName (autName a) ln
     toLoc = [ t | t <- transitions a, (end t)==ln]
     fromLoc = [ t | t <- transitions a, (start t)==ln]
     addUpdates t = case (elem t toLoc, elem t fromLoc) of
                         (True, False) -> addUpdate (AssignInt vn (IntConst 1)) t
                         (False, True) -> addUpdate (AssignInt vn (IntConst 0)) t
                         (_,_) -> t
 in a { transitions = map addUpdates (transitions a) }

invariantToGuards :: L SynchCircuit -> Invariant -> Maybe Strengthening
invariantToGuards m outrefs = 
 do
  let (inputs, nonInputs) = partition isInputGate outrefs
  activeInput <- find isNegative inputs
  (SCInp activeEvent) <- (lookupInp m) activeInput
  let locEntries = [x | Just x <- map (lookupLoc m) nonInputs]
      varEntries = [x | Just x <- map (lookupVar m) nonInputs]
      locEntriesByAut = groupWith (\(SCLoc n _ _) -> n) locEntries
      varEntriesByVar = groupWith (\(SCVar n _ _) -> n) varEntries
      varGuardsByVar = map createVarGuards varEntriesByVar
      relevantLocEntries = filter (\(SCLoc _ _ p) -> not p) locEntries
                           --(map . filter) (\(SCLoc _ _ p) -> not p) locEntriesByAut
      varGuards' = concat varGuardsByVar
      (locsToStrengthen,newVars') = unzip [((n,l),newLocVarName n l)
                                          | (SCLoc n l _) <- relevantLocEntries
                                          ]
      locGuards' =  [( n
                     , l
                     , GInt (if (not pol) then NEquals else Equals)
                           (newLocVarName n l)
                           (IntConst 1)
                     )
                   | (SCLoc n l pol) <- relevantLocEntries
                   ]
      strth = 
       Strengthen { strLocs = locsToStrengthen
                  , strEvent = activeEvent
                  , strVarGuards = varGuards'
                  , strLocGuards = locGuards'
                  -- if (varGuards==[]) then [Bottom] else varGuards
                  , newVars = newVars'
                  }
  return strth


newLocVarName :: Name -> Location -> VarName
newLocVarName n l = n ++ "@" ++ l

newLocVar :: Automaton -> Location -> Variable
newLocVar a l = Variable { lower = 0
                         , upper = 1
                         , initial = if (initialLocation a)==l then 1 else 0
                         }


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
  adjustGate n (NOut x) = NOut $ adjustGate n x
  adjustGate n x = x

maxGateNo :: [OutputRef] -> Int
maxGateNo = foldl (flip (max . getNbr)) (-1)




-- Precondition: the entire list deals with the same variable
createVarGuards :: [SCVar] -> [Guard]
createVarGuards scvs = map createVarGuard scvs
                       -- combineGuardList $ map createVarGuard scvs

createVarGuard :: SCVar -> Guard
createVarGuard (SCVar vn i pol) =
 GInt (if (not pol) then LessThan else GreaterThanEq) vn (IntConst i)



--type SCLookup = OutputRef -> SCLookupEntry

data SCInp = SCInp Event
   deriving ( Show )
data SCVar = SCVar VarName Int Bool
   deriving ( Show )
data SCLoc = SCLoc Name Location Bool
   deriving ( Show )
   


{--
isSCLVar :: SCLookupEntry -> Bool
isSCLVar (SCVar _ _) = True
isSCLVar _ = False
--}



lookupInp :: L SynchCircuit -> OutputRef -> Maybe SCInp
lookupInp m r = lookup rname inputs
 where
  rname = toName r
  (sc, gs) = run m
  inputEntries = map (SCInp . fst) (eventRefs sc)
  inputRefs  = [ n | (n, Input) <- gs ]
  inputs = zip inputRefs inputEntries


lookupVar :: L SynchCircuit -> OutputRef -> Maybe SCVar
lookupVar m r =
 do
  v <- lookup rname flops
  case (v) of
       (SCVar "" 0 False) -> Nothing
       (_) -> Just v
 where
  rname = toName r
  (sc, gs) = run m
  flopRefs   = [ n | (n, Flop init x) <- gs ]
  locEntries = [ SCVar "" 0 False
               | _ <- concat [ locsToAut
                             | (_, locsToAut) <- locRefs sc
                             ]
               ]
  varEntries = [ SCVar vname val (not $ isNegative r)
               | (vname, val, _) <-
                 concat [ zip3 (repeat vname) [(offset+1)..] vrefs
                        | (vname, (vrefs, offset)) <- varRefs sc
                        ]
               ]
  flopEntries = locEntries ++ varEntries
  flops = zip flopRefs flopEntries

  
lookupLoc :: L SynchCircuit -> OutputRef -> Maybe SCLoc
lookupLoc m r = lookup rname list
 where
  rname = toName r
  (sc, gs) = run m
  list = flops
  flopRefs   = [ n | (n, Flop init x) <- gs ]
  locEntries = [SCLoc aut loc (not $ isNegative r)
               | (aut, loc) <-
                  concat [ zip (repeat aut) (map fst locsToAut)
                         | (aut, locsToAut) <- locRefs sc
                         ]
               ]
  flops = zip flopRefs locEntries





