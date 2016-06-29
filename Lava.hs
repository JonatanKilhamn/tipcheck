module Lava where

import Control.Monad( ap )
import Circuit

--------------------------------------------------------------------------------
-- mini-Lava

newtype L a = L (Int -> (a, Int, [(Name,Gate)]))

data Gate
  = Input
  | Flop (Maybe Bool) Ref
  | And Ref Ref
 deriving ( Show )

instance Applicative L where
  pure  = return
  (<*>) = ap

instance Functor L where
  fmap f (L m) =
    L (\n0 -> let (x, n1, gs) = m n0
               in (f x, n1, gs))

instance Monad L where
  return x =
    L (\n -> (x, n, []))
  
  L m0 >>= k =
    L (\n0 -> let (x, n1, gs1) = m0 n0
                  L m1         = k x
                  (y, n2, gs2) = m1 n1
               in (y, n2, gs1++gs2))

-- primitives

run :: L a -> (a, [(Name,Gate)])
run (L m) = (x, gs)
 where
  (x, _, gs) = m 0

and2 :: Ref -> Ref -> L Ref
and2 x y =
  L (\n -> let z = "X" ++ show n
            in n `seq` (Pos z, n+1, [(z, And x y)]))

input :: L Ref
input =
  L (\n -> let z     = "I" ++ show n
            in n `seq` (Pos z, n+1, [(z, Input)]))
            
logL :: String -> L ()
logL msg =
  L (\n -> n `seq` ((), n, [(msg, Input)]))

flop :: Maybe Bool -> L (Ref, Ref -> L ())
flop init =
  L (\n -> let z     = "R" ++ show n
               def x = L (\n -> ((), n, [(z, Flop init x)]))
            in n `seq` ((Pos z,def), n+1, []))

--------------------------------------------------------------------------------
-- derived gates

or2, impl2, xor2, eq2 :: Ref -> Ref -> L Ref
or2   x y = neg `fmap` and2 (neg x) (neg y)
impl2 x y = or2 (neg x) y
eq2   x y = xor2 (neg x) y
xor2  x y =
  do a <- and2 x (neg y)
     b <- and2 (neg x) y
     or2 a b

mux :: Ref -> (Ref,Ref) -> L Ref
mux c (a,b) =
  do ca <- and2 (neg c) a
     cb <- and2 c b
     or2 ca cb

andl, orl :: [Ref] -> L Ref
orl xs = neg `fmap` andl (map neg xs)
andl [] =
  do return tt
  
andl [x] =
  do return x
  
andl xs =
  do a <- andl (take k xs)
     b <- andl (drop k xs)
     and2 a b
 where
  k = length xs `div` 2

eql :: [Ref] -> [Ref] -> L Ref
eql [] ys =
  do andl (map neg ys)
  
eql xs [] =
  do andl (map neg xs)
  
eql (x:xs) (y:ys) =
  do e <- eq2 x y
     es <- eql xs ys
     and2 e es
     
multi_xor :: [Ref] -> L Ref
multi_xor [] = return ff
multi_xor (x:xs) =
 do a <- multi_xor xs
    xor2 x a

flop0, flop1, flopX :: L (Ref, Ref -> L ())
flop0 = flop (Just False)
flop1 = flop (Just True)
flopX = flop Nothing

--------------------------------------------------------------------------------
-- liveness combinators

finitely :: Ref -> L Ref
finitely x =
  do chal <- input

     (wait,next_wait) <- flop1
     wait' <- and2 wait (neg chal)
     next_wait wait'
     
     (finx,next_finx) <- flop0
     finx1' <- and2 wait chal
     finx2' <- and2 finx (neg x)
     finx'  <- or2 finx1' finx2'
     next_finx finx'
     
     return finx'

eventually :: Ref -> L Ref
eventually x =
  do (hap, next_hap) <- flop0
     hap' <- or2 x hap
     next_hap hap'
     return hap'

never :: Ref -> L Ref
never x = neg `fmap` eventually x

cnst :: L Ref
cnst =
  do (x, def_x) <- flopX
     def_x x
     return x

--------------------------------------------------------------------------------
-- props

data Props
  = Props
  { always     :: [Ref]
  , nevers     :: [Ref]
  , infinitely :: [Ref]
  , finites    :: [[Ref]]
  }
 deriving ( Show )

props :: Props
props = Props [] [] [] []

circuit :: L Props -> Circuit
circuit m = circ
 where
  (props, gs) = run m
 
  circ = Circuit
    { inputs  = [ n | (n, Input) <- gs ]
    , flops   = [ (n,(init,x)) | (n, Flop init x) <- gs ]
    , gates   = [ (n, x, y) | (n, And x y) <- gs ]
    , constrs = always     props
    , bads    = nevers     props
    , fairs   = infinitely props
    , justs   = finites    props
    }

--------------------------------------------------------------------------------

