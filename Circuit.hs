module Circuit where

import Test.QuickCheck
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Control.Monad
import System.IO

--------------------------------------------------------------------------------

type Name = String

data Ref
  = Bool Bool
  | Pos Name
  | Neg Name
 deriving ( Eq )

instance Show Ref where
  show (Bool False) = "0"
  show (Bool True)  = "1"
  show (Pos x)      = x
  show (Neg x)      = "~" ++ x

data Circuit
  = Circuit
  { inputs  :: [Name]
  , flops   :: [(Name,(Maybe Bool, Ref))]
  , constrs :: [Ref]
  , bads    :: [Ref]
  , fairs   :: [Ref]
  , justs   :: [[Ref]]
  , gates   :: [(Name,Ref,Ref)]
  }

instance Show Circuit where
  show circ = unlines $
    [ "INPUTS " ++ commas (inputs circ) ++ ";"
    , "FLOPS " ++ commas (map fst (flops circ)) ++ ";"
    ] ++
    [ "GATES"
    | not (null (gates circ))
    ] ++
    [ "  " ++ x ++ " = " ++ show y ++ " & " ++ show z ++ ";"
    | (x,y,z) <- gates circ
    ] ++
    [ "FLOPDEFS"
    | not (null (flops circ))
    ] ++
    [ "  " ++ x ++ " := (" ++ inits ++ ") " ++ show next ++ ";"
    | (x,(init,next)) <- flops circ
    , let inits = case init of
                    Nothing    -> "?"
                    Just False -> "0"
                    Just True  -> "1"
    ] ++
    [ "ASSUME " ++ commas (map show (constrs circ)) ++ ";"
    , "BADS " ++ commas (map show (bads circ)) ++ ";"
    , "FAIRS " ++ commas (map show (fairs circ)) ++ ";"
    , "JUSTIFY " ++ commas [ "{" ++ commas (map show js) ++ "}" | js <- justs circ ] ++ ";"
    ]
   where
    commas = concat . intersperse ", " 

--------------------------------------------------------------------------------
-- inductive testing helper functions

step :: Circuit -> [Bool] -> [Bool] -> (Bool, [Bool], Circuit)
step circ sin' inps = (pconstr, pbads, circ')
 where
  circ' =
    circ{ flops = [ (x,(Just init',next))
                  | ((x,(_,next)),init') <- flops circ `zip` sout
                  ]
        }
  
  pconstr =
    and [ val x state
        | x <- constrs circ
        ]

  pbads =
    [ val x state
    | x <- bads circ
    ]

  state0 =
    M.fromList $
    [ (x, v)
    | ((x,(init,_)),init') <- flops circ `zip` sin'
    , let v = case init of
                Nothing -> init'
                Just i  -> i
    ] ++
    [ (x, i)
    | (x, i) <- inputs circ `zip` inps
    ]

  state = foldl eval state0 (gates circ)
   where
    eval st (x,a,b) =
      M.insert x (val a st && val b st) st
  
  sout =
    [ val next state
    | (_,(_,next)) <- flops circ
    ]
    
  val (Pos x) st =
    case M.lookup x st of
      Just v -> v
  val (Neg x) st =
    not (val (Pos x) st)
  val (Bool b) st =
    b

double :: String -> Circuit -> Circuit
double tag circ =
  Circuit
  { inputs  = inputs circ
           ++ [ pr i | i <- inputs circ ]
  , flops   = [ (x,(i,prime x')) | (x,(i,x')) <- flops circ ]
  , gates   = gates circ
           ++ [ (pr x,x',x') | (x,(_,x')) <- flops circ ]
           ++ [ (pr x,prime a,prime b) | (x,a,b) <- gates circ ]
           ++ [ (prop ++ "_" ++ show i,neg b,neg (prime b)) | (b,i) <- bads circ `zip` [1..] ]
  , constrs = constrs circ
           ++ map prime (constrs circ)
  , bads    = [ Neg (prop ++ "_" ++ show i) | (b,i) <- bads circ `zip` [1..] ]
  , fairs   = [] -- for now
  , justs   = [] -- for now
  }
 where
  pr x = x ++ "_" ++ tag
  prop = "prop_" ++ tag
  
  prime (Pos x) = Pos (pr x)
  prime (Neg x) = Neg (pr x)
  prime b       = b

time0 :: Circuit -> Circuit
time0 circ =
  Circuit
  { inputs  = inputs circ
           ++ [ x | (x,(Nothing,_)) <- flops circ ]
  , flops   = []
  , gates   = [ (x,Bool b,Bool b) | (x,(Just b,_)) <- flops circ ]
           ++ gates circ
  , constrs = constrs circ
  , bads    = bads circ
  , fairs   = [] -- is OK!
  , justs   = [] -- is OK!
  }

--------------------------------------------------------------------------------

ff, tt :: Ref
ff = Bool False
tt = Bool True

neg :: Ref -> Ref
neg (Pos x)  = Neg x
neg (Neg x)  = Pos x
neg (Bool b) = Bool (not b)

--------------------------------------------------------------------------------

writeCircuit :: FilePath -> Circuit -> IO ()
writeCircuit file circ =
  do h <- (s==s) `seq` openBinaryFile file WriteMode
     hPutStr h s
     hClose h
 where
  -- table
  table =
    M.fromList $
    ( inputs circ
   ++ map fst (flops circ)
   ++ [ x | (x,_,_) <- gates circ ]
    ) `zip` [2 :: Int,4..]
 
  name x = fromJust (M.lookup x table)
  
  ref (Bool True)  = 1
  ref (Bool False) = 0
  ref (Pos x)      = name x
  ref (Neg x)      = name x + 1
  
  -- "aig M I L O A B C J F"
  miloabcjf =
    [ maximum (1 : map snd (M.toList table)) `div` 2
    , length (inputs circ)
    , length (flops circ)
    , 0 -- no outputs
    , length (gates circ)
    , length (bads circ)
    , length (constrs circ)
    , length (justs circ)
    , length (fairs circ)
    ]
  
  -- gates
  gate (x', a', b') = 
    bin (x-a1) ++ bin (a1-b1)
   where
    x = ref (Pos x')
    a = ref a'
    b = ref b'
    
    a1 = max a b
    b1 = min a b

    bin n | n < 0     = error "negative argument to bin"
          | n < 128   = [chr n]
          | otherwise = chr (128 + (n `mod` 128)) : bin (n `div` 128)

  -- representation
  s = unlines
    ( -- header
      [ unwords ("aig" : map show miloabcjf) ]
      -- flop definitions
   ++ [ show (ref x') ++ " " ++
          case mi of
            Nothing -> show (ref (Pos x))
            Just b  -> show (ref (Bool b))
      | (x,(mi,x')) <- flops circ
      ]
      -- bads, constrs, justs, fairs
   ++ [ show (ref x) | x <- bads circ ]
   ++ [ show (ref x) | x <- constrs circ ]
   ++ [ show (length xs) | xs <- justs circ ]
   ++ [ show (ref x) | xs <- justs circ, x <- xs ]
   ++ [ show (ref x) | x <- fairs circ ]
    ) -- gate definitions
   ++ concat [ gate g | g <- gates circ ]

--------------------------------------------------------------------------------
-- generation

arbCircuit :: Int -> Int -> Int -> (Int,Int,Int,Int) -> Gen Circuit
arbCircuit numInps numFlops numGates (numConstrs,numBads,numFairs,numJusts) =
  do (pts,gates_) <- arbGates 1 numGates pts0
     
     flops_   <- sequence [ liftM2 (,) arbitrary (point pts) | i <- [1..numFlops] ]
     constrs_ <- sequence [ point pts | i <- [1..numConstrs] ]
     bads_    <- sequence [ point pts | i <- [1..numBads] ]
     fairs_   <- sequence [ point pts | i <- [1..numFairs] ]
     justs_   <- sequence [ do k <- choose (0,4::Int)
                               sequence [ point pts | j <- [1..k] ]
                          | i <- [1..numJusts]
                          ]

     return Circuit
       { inputs  = inpNames
       , flops   = flopNames `zip` flops_
       , constrs = constrs_
       , bads    = bads_
       , fairs   = fairs_
       , justs   = justs_
       , gates   = gates_
       }
 where
  inpNames  = [ "i" ++ show i | i <- [1..numInps] ]
  flopNames = [ "r" ++ show i | i <- [1..numFlops] ]
  pts0      = [ Pos x | x <- inpNames ++ flopNames ] ++ [ff]
  
  point pts = do p <- frequency (freqs `zip` map return (reverse pts))
                 b <- arbitrary
                 return (if b then p else neg p)
   where
    freqs = [50..]

  arbGates i 0 pts =
    do return (pts,[])
  
  arbGates i n pts =
    frequency
    [ (3, do x <- point pts
             y <- point pts
             (pts',gs) <- arbGates (i+1) (n-1) (Pos z : pts)
             return (pts',(z,x,y):gs))
    , (1, do x <- point pts
             y <- point pts
             (pts',gs) <- arbGates (i+1) (n-1) (Pos z : Pos a : Pos b : pts)
             return (pts',(a,neg x,y):(b,x,neg y):(z,Neg a,Neg b):gs))
    ]
   where
    z = "x" ++ show i
    a = "xa" ++ show i
    b = "xb" ++ show i

instance Arbitrary Circuit where
  arbitrary =
    sized $ \size ->
      let n = size `div` 5 + 1 in
      do numInps  <- choose (0,n)
         numFlops <- choose (0,n)
         numGates <- choose (0,10*n)
       
         numConstrs <- choose (0,n `div` 2 + 1)
         numBads    <- choose (0,n)
         numFairs   <- choose (0,n `div` 2 + 1)
         numJusts   <- choose (0,n)
       
         arbCircuit numInps numFlops numGates (numConstrs,numBads,numFairs,numJusts)

  shrink circ =
       removeConstrs circ
    ++ removeFairs circ
    ++ removeJusts circ
    ++ removeBads circ
    ++ removeManyGates circ
    ++ removeInputsFlops circ
    ++ removeGates circ
    ++ initializeFlops circ
   where
    removeConstrs circ =
      [ circ{ constrs = constrs circ \\ [x] }
      | x <- constrs circ
      ] ++
      [ circ{ constrs = constrs circ \\ [x]
            , gates   = gates circ ++ impls
            , bads    = bads'
            }
      | x <- constrs circ
      , let news  = take (length (bads circ)) ([ "y" ++ show i | i <- [1..] ] \\ [ x | (x,_,_) <- gates circ ])
            bads' = [ Neg y | y <- news ]
            impls = [ (y,x,neg b) | (y,b) <- news `zip` bads circ ]
      ]

    removeBads circ =
      [ circ{ bads = bads circ \\ [x] }
      | x <- bads circ
      ] 

    removeFairs circ =
      [ circ{ fairs = fairs circ \\ [x] }
      | x <- fairs circ
      ] ++
      [ circ{ fairs = fairs circ \\ [x]
            , justs = [ x:xs | xs <- justs circ ]
            }
      | x <- fairs circ
      ]

    removeJusts circ =
      [ circ{ justs = justs circ \\ [xs] }
      | xs <- justs circ
      ] ++
      [ circ{ justs = take i (justs circ)
                   ++ [(justs circ !! i) \\ [x]]
                   ++ drop (i+1) (justs circ)
            }
      | i <- [0..length (justs circ)-1]
      , x <- justs circ !! i
      ] ++
      [ circ{ justs = justs circ \\ [xs]
            , bads  = x : bads circ
            }
      | xs <- justs circ
      , length xs <= 1
      , let x = case xs of
                  []  -> tt
                  [x] -> x
      ]

    removeInputsFlops circ =
      [ replace x b circ
      | x <- inputs circ ++ map fst (flops circ)
      , b <- [ ff, tt ]
      ] ++
      [ replace x y' circ
      | x <- inputs circ ++ map fst (flops circ)
      , y <- takeWhile (/=x) (inputs circ ++ map fst (flops circ))
      , y' <- [Pos y, Neg y]
      ] ++
      [ circ{ inputs = x : inputs circ
            , flops  = [ (y,w) | (y,w) <- flops circ, y /= x ]
            }
      | (x,_) <- flops circ
      ]

    removeManyGates circ =
      [ reps gones circ
      | gates' <- shrinkList (gates circ)
      , let gones  = [ x | (x,_,_) <- gates circ, x `notElem` [ x | (x,_,_) <- gates' ] ]

            reps []     = id
            reps (x:xs) = replace x ff . reps xs
      , _:_:_ <- [gones]
      ]
     where
      shrinkList []  = []
      shrinkList [x] = [[]]
      shrinkList xs  = [ as, bs ]
                    ++ ( [ as ++ bs' | bs' <- shrinkList bs ]
                   `ilv` [ as' ++ bs | as' <- shrinkList as ]
                       )
       where
        k  = length xs `div` 2
        as = take k xs
        bs = drop k xs

        []     `ilv` ys = ys
        (x:xs) `ilv` ys = x : (ys `ilv` xs)

    removeGates circ =
      [ replace x c circ
      | (x,a,b) <- gates circ
      , let ys = [a,b,ff]
              ++ map Pos ( inputs circ
                        ++ map fst (flops circ)
                        ++ takeWhile (/=x) [ y | (y,_,_) <- gates circ ]
                         )
      , c <- nub (ys ++ map neg ys)
      ]

    initializeFlops circ =
      [ circ
        { flops = take i (flops circ)
               ++ [ (x,(Just b,y)) ]
               ++ drop (i+1) (flops circ)
        }
      | ((x,(Nothing,y)),i) <- flops circ `zip` [0..]
      , b <- [False,True]
      ]

    replace x c circ =
      Circuit
      { inputs  = inputs circ \\ [x]
      , flops   = [ (y,(init,rep y')) | (y,(init,y')) <- flops circ, x /= y ]
      , constrs = map rep (constrs circ)
      , bads    = map rep (bads circ)
      , fairs   = map rep (fairs circ)
      , justs   = map (map rep) (justs circ)
      , gates   = [ (z, rep a, rep b)
                  | (z,a,b) <- gates circ
                  , z /= x
                  ]
      }
     where
      rep (Pos y) | x == y = c
      rep (Neg y) | x == y = neg c
      rep a                = a

--------------------------------------------------------------------------------
-- circuit modifiers

data Yes = Yes
data No  = No

class Choice c where
  make   :: c
  choice :: c -> a -> a -> a

instance Choice Yes where
  make = Yes
  choice _ yes no = yes
  
instance Choice No where
  make = No
  choice _ yes no = no

makeChoice :: Choice c => Gen c
makeChoice = return make

data ModCircuit constrs safes fairs lives
  = Mod (constrs,safes,fairs,lives) Circuit

instance Show (ModCircuit constrs safes fairs lives) where
  show (Mod _ c) = show c

instance (Choice constrs, Choice safes, Choice fairs, Choice lives)
      => Arbitrary (ModCircuit constrs safes fairs lives) where
  arbitrary =
    do circ <- arbitrary
       hasConstrs <- makeChoice
       hasSafes   <- makeChoice
       hasFairs   <- makeChoice
       hasLives   <- makeChoice
       return $
         Mod (hasConstrs,hasSafes,hasFairs,hasLives)
           circ
           { constrs = choice hasConstrs (constrs circ) []
           , bads    = choice hasSafes   (bads circ)    []
           , fairs   = choice hasFairs   (fairs circ)   []
           , justs   = choice hasLives   (justs circ)   []
           }

  shrink (Mod c@(hasConstrs,hasSafes,hasFairs,hasLives) circ) =
    [ Mod c circ'
    | circ' <- shrink circ
    , choice hasConstrs True (null (constrs circ'))
    , choice hasSafes   True (null (bads circ'))
    , choice hasFairs   True (null (fairs circ'))
    , choice hasLives   True (null (justs circ'))
    ]

--------------------------------------------------------------------------------

