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
  , flops   :: [Name]
  , flops'  :: [(Ref, Maybe Bool)]
  , constrs :: [Ref]
  , bads    :: [Ref]
  , fairs   :: [Ref]
  , justs   :: [[Ref]]
  , gates   :: [(Name,Ref,Ref)]
  }

instance Show Circuit where
  show circ = unlines $
    [ "INPUTS " ++ commas (inputs circ) ++ ";"
    , "FLOPS " ++ commas (flops circ) ++ ";"
    , "GATES"
    ] ++
    [ "  " ++ x ++ " = " ++ show y ++ " & " ++ show z ++ ";"
    | (x,y,z) <- gates circ
    ] ++
    [ "FLOPDEFS"
    | not (null (flops circ))
    ] ++
    concat
    [ [ "  init(" ++ x ++ ") = " ++ inits ++ ";"
      , "  next(" ++ x ++ ") = " ++ show next ++ ";"
      ]
    | (x,(next,init)) <- flops circ `zip` flops' circ
    , let inits = case init of
                    Nothing    -> "?"
                    Just False -> "0"
                    Just True  -> "1"
    ] ++
    [ "ASSUME " ++ commas (map show (constrs circ)) ++ ";"
    , "BADS " ++ commas (map show (bads circ)) ++ ";"
    , "FAIRS " ++ commas (map show (fairs circ)) ++ ";"
    , "JUSTIFY " ++ concat (intersperse " " [ "{" ++ commas (map show js) ++ "}" | js <- justs circ ]) ++ ";"
    ]
   where
    commas = concat . intersperse ", " 

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
  do h <- openBinaryFile file WriteMode
     hPutStr h s
     hClose h
 where
  -- table
  table =
    M.fromList $
    ( inputs circ
   ++ flops circ
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

    bin n | n < 128   = [chr n]
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
      | (x,(x',mi)) <- flops circ `zip` flops' circ
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
     
     flops_   <- sequence [ liftM2 (,) (point pts) arbitrary | i <- [1..numFlops] ]
     constrs_ <- sequence [ point pts | i <- [1..numConstrs] ]
     bads_    <- sequence [ point pts | i <- [1..numBads] ]
     fairs_   <- sequence [ point pts | i <- [1..numFairs] ]
     justs_   <- sequence [ do k <- choose (1,3::Int)
                               sequence [ point pts | j <- [1..k] ]
                          | i <- [1..numJusts]
                          ]

     return Circuit
       { inputs  = inpNames
       , flops   = flopNames
       , flops'  = flops_
       , constrs = constrs_
       , bads    = bads_
       , fairs   = fairs_
       , justs   = justs_
       , gates   = gates_
       }
 where
  inpNames  = [ "I" ++ show i | i <- [1..numInps] ]
  flopNames = [ "R" ++ show i | i <- [1..numFlops] ]
  pts0      = [ Pos x | x <- inpNames ++ flopNames ] ++ [ff]
  
  point pts = do p <- frequency (freqs `zip` map return (reverse pts))
                 b <- arbitrary
                 return (if b then p else neg p)
   where
    freqs = [10..]

  arbGates i 0 pts =
    do return (pts,[])
  
  arbGates i n pts =
    do x <- point pts
       y <- point pts
       (pts',gs) <- arbGates (i+1) (n-1) (Pos z : pts)
       return (pts',(z,x,y):gs)
   where
    z = "X" ++ show i

instance Arbitrary Circuit where
  arbitrary =
    sized $ \n ->
      do numInps  <- choose (0,n)
         numFlops <- choose (0,n)
         numGates <- choose (0,10*n)
       
         numConstrs <- choose (0,n `div` 10 + 1)
         numBads    <- choose (0,n `div` 10 + 1)
         numFairs   <- choose (0,n `div` 10 + 1)
         numJusts   <- choose (0,n `div` 10 + 1)
       
         arbCircuit numInps numFlops numGates (numConstrs,numBads,numFairs,numJusts)

  shrink circ =
       removeConstrs circ
    ++ removeBads circ
    ++ removeFairs circ
    ++ removeJusts circ
    ++ removeInputsFlops circ
    ++ removeManyGates circ
    ++ removeGates circ
    ++ replaceFlopsByInputs circ
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
      , let news  = take (length (bads circ)) ([ "Y" ++ show i | i <- [1..] ] \\ [ x | (x,_,_) <- gates circ ])
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
      | x <- inputs circ ++ flops circ
      , b <- [ ff, tt ]
      ] ++
      [ replace x y' circ
      | x <- inputs circ ++ flops circ
      , y <- inputs circ ++ flops circ
      , y < x
      , y' <- [Pos y, Neg y]
      ]

    removeManyGates circ =
      [ reps gones circ
      | gates' <- shrinkList (gates circ)
      , let gones  = [ x | (x,_,_) <- gates circ, x `notElem` [ x | (x,_,_) <- gates' ] ]

            reps []     = id
            reps (x:xs) = replace x ff . reps xs
      ]
     where
      shrinkList []  = []
      shrinkList [x] = [[]]
      shrinkList xs  = [ as, bs ]
                    ++ [ as ++ bs' | bs' <- shrinkList bs ]
                    ++ [ as' ++ bs | as' <- shrinkList as ]
       where
        k = length xs `div` 2
        as = take k xs
        bs = drop k xs

    removeGates circ =
      [ replace x c circ
      | (x,a,b) <- gates circ
      , c <- [ a, b ]
      ]

    replaceFlopsByInputs circ =
      [ circ
        { inputs = inputs circ ++ [x]
        , flops  = flops circ \\ [x]
        , flops' = [ flp | (y,flp) <- flops circ `zip` flops' circ, x /= y ]
        }
      | x <- flops circ
      ]

    initializeFlops circ =
      [ circ
        { flops' = take i (flops' circ)
                ++ [ (y,Just b) ]
                ++ drop (i+1) (flops' circ)
        }
      | ((y,Nothing),i) <- flops' circ `zip` [0..]
      , b <- [False,True]
      ]

    replace x c circ =
      Circuit
      { inputs  = inputs circ \\ [x]
      , flops   = flops circ  \\ [x]
      , flops'  = [ (rep y',init) | (y,(y',init)) <- flops circ `zip` flops' circ, x /= y ]
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

