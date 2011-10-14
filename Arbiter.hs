module Main where

import Circuit
import Lava

--------------------------------------------------------------------------------
-- arbiters

arb2 :: (Ref,Ref) -> L (Ref,Ref)
arb2 (r1,r2) =
  do (x,next_x) <- flopX
     swap_x <- and2 r1 r2
     x' <- xor2 x swap_x
     next_x x'
     
     w1 <- and2 r2 x
     q1 <- or2 (neg r2) w1
     a1 <- and2 r1 q1
     
     w2 <- and2 r1 (neg x)
     q2 <- or2 (neg r1) w2
     a2 <- and2 r2 q2

     return (a1,a2)

arb2_props :: L Props
arb2_props =
  do -- the circuit
     r1 <- input
     r2 <- input
     (a1,a2) <- arb2 (r1,r2)
  
     -- never 2 acks
     b1 <- and2 a1 a2
     
     -- always some ack for some req
     ack <- or2 a1 a2
     req <- or2 r1 r2
     b2  <- and2 req (neg ack)
     
     -- never an ack to someone who did not req
     b3 <- and2 a1 (neg r1)
     b4 <- and2 a2 (neg r2)

     -- not just a finite number of a1 and a2
     fin_a1 <- finitely a1
     fin_a2 <- finitely a2
   
     -- props
     return $ props
       { nevers  = [b1,b2,b3,b4]
       , finites = [ [r1,fin_a1]
                   , [r2,fin_a2]
                   ]
       }

--------------------------------------------------------------------------------

arbiter :: Maybe Bool -> Int -> [Ref] -> L [Ref]
arbiter init offset rs =
  do -- create counters
     counters <- sequence [ sequence [ flop init | i <- [1..k] ] | r <- rs ]
     let cs = [ [ x | (x,_) <- counter ] | counter <- counters ]
     
     -- compute ack signals
     (ms,_) <- pick [ c ++ [r] | (c,r) <- cs `zip` rs ]
     as     <- sequence [ and2 r m | (r,m) <- rs `zip` ms ]
     
     -- compute next counter values
     sequence_
       [ do incr <- and2 r (neg a)
            c'   <- count a incr c
            sequence_ [ next_x x | (next_x,x) <- next_c `zip` c' ]
       | (counter,(r,a)) <- counters `zip` (rs `zip` as)
       , let (c,next_c) = unzip counter
       ]
     
     -- return acks
     return as
 where
  k = head [ k | (k,n) <- [0..] `zip` iterate (*2) 1, n >= length rs ] - offset

pick :: [Bin] -> L ([Ref],Bin)
pick [w] =
  do return ([tt],w)

pick xs =
  do (as1,w1) <- pick (take k xs)
     (as2,w2) <- pick (drop k xs)
     (lte,_)  <- comp w1 w2
     w        <- sequence [ mux lte (x,y) | (x,y) <- w1 `zip` w2 ]
     as1' <- sequence [ and2 (neg lte) a | a <- as1 ]
     as2' <- sequence [ and2 lte a       | a <- as2 ]
     return (as1'++as2',w)
 where
  k = length xs `div` 2

comp :: Bin -> Bin -> L (Ref,Ref)
comp [] [] =
  do return (tt,tt)

comp (x:xs) (y:ys) =
  do (lte,eq) <- comp xs ys
     ltexy <- impl2 x y
     eqxy  <- eq2 x y
     eq'   <- and2 eqxy eq
     lte'  <- mux eq (lte, ltexy)
     return (lte',eq')

type Bin = [Ref]

count :: Ref -> Ref -> Bin -> L Bin
count reset incr xs =
  do xs1 <- bitAdder incr xs
     sequence [ and2 x (neg reset) | x <- xs1 ]
 where
  bitAdder incr [] =
    do return []
  
  bitAdder incr (x:xs) =
    do (s,c) <- halfAdd incr x
       ss    <- bitAdder c xs
       return (s:ss)
  
  halfAdd a b =
    do s <- xor2 a b
       c <- and2 a b
       return (s,c)

arbiter_props :: Maybe Bool -> Int -> Int -> L Props
arbiter_props init offset n =
  do -- the circuit
     rs <- sequence [ input | i <- [1..n] ]
     as <- arbiter init offset rs
     if length as /= n then error "length!" else return ()
  
     -- never 2 acks
     two_acks <- sequence
                 [ and2 (as!!i) (as!!j)
                 | i <- [0..n-1]
                 , j <- [i+1..n-1]
                 ]
     b1 <- orl two_acks
     
     -- always some ack for some req
     ack <- orl as
     req <- orl rs
     b2  <- and2 req (neg ack)
     
     -- never an ack to someone who did not req
     no_reqs <- sequence
                [ and2 a (neg r)
                | (a,r) <- as `zip` rs
                ]
     b3 <- orl no_reqs

     -- not just a finite number of a1 and a2
     fin_as <- sequence [ finitely a | a <- as ]
   
     -- props
     return $ props
       { nevers  = [b1,b2,b3]
       , finites = [ [r,fin_a]
                   | (r,fin_a) <- rs `zip` fin_as
                   ]
       }

--------------------------------------------------------------------------------

main :: IO ()
main =
  do sequence_ [ do putStrLn (show n ++ "/" ++ show maxN ++ "...")
                    writeCircuit ("Examples/arbiter_init0_" ++ sn ++ ".aig")
                                 (circuit (arbiter_props (Just False) 0 n))
                    writeCircuit ("Examples/arbiter_initX_" ++ sn ++ ".aig")
                                 (circuit (arbiter_props Nothing 0 n))
                    writeCircuit ("Examples/arbiter_init0_" ++ sn ++ "_bug.aig")
                                 (circuit (arbiter_props (Just False) 1 n))
                    writeCircuit ("Examples/arbiter_initX_" ++ sn ++ "_bug.aig")
                                (circuit (arbiter_props Nothing 1 n))
               | n <- [2..maxN]
               , let sn | n < 10    = "0" ++ show n
                        | otherwise = show n
               ]
 where
  maxN = 64

--------------------------------------------------------------------------------

