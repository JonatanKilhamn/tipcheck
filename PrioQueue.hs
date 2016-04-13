module Main where

import Circuit
import Lava

--------------------------------------------------------------------------------
-- priority queues

type Bin = [Ref]

binflop :: Maybe Bool -> Int -> L (Bin, Bin -> L ())
binflop init k =
  do flps <- sequence [ flop init | i <- [1..k] ]
     return ( map fst flps
            , \xs -> sequence_ [ def x | (x,def) <- xs `zip` map snd flps ]
            )

prioqueue :: Maybe Bool -> Int -> Bin -> L Bin
prioqueue init n inp =
  do flps <- sequence [ binflop init k | i <- [1..n] ]
     outs <- insert (inp : map fst flps)
     sequence_ [ def out | (out,def) <- outs `zip` map snd flps ]
     return (last outs)
 where
  k = length inp

  insert (a:b:as) =
    do (a',b') <- cmpswap ff ff (a,b)
       bs <- insert (b':as)
       return (a':bs)
  
  insert as =
    do return as

  cmpswap lft rgt (xs,ys) =
    do return (xs,ys)

{-
  cmpswap lft rgt ([],[]) =
    do return ([],[])
    
  cmpswap lft rgt (x:xs,y:ys) =
    do undec <- and2 (neg lft) (neg rgt)
       lftb  <- andl [undec, neg x, y]
       rgtb  <- andl [undec, x, neg y]
       lft'  <- or2 lft lftb
       rgt'  <- or2 rgt rgtb
       swap  <- and2 x (neg y)
       swpx  <- mux swap (y,x)
       swpy  <- mux swap (x,y)
       x'    <- mux lft' (x, swpx)
       y'    <- mux rgt' (y, swpy)
       (xs',ys') <- cmpswap lft' rgt' (xs,ys)
       return (x':xs',y':ys')
-}

prio_props :: Maybe Bool -> Int -> Int -> L Props
prio_props init n k =
  do -- the circuit
     inp <- sequence [ input | i <- [1..k] ]
     out <- prioqueue init n inp
  
     -- the data we track
     track <- sequence [ return ff {- cnst -} | i <- [1..k] ]
     
     -- we complain if these conditions hold simultaneously
     inEvent <- eql track inp
     just1 <- eventually inEvent
     
     outEvent <- eql track out
     just2 <- never outEvent

     -- the props
     return $ props
       { finites = [ [just1, just2] ]
       }

--------------------------------------------------------------------------------

main :: IO ()
main =
  do sequence_ [ do putStrLn (show n ++ "/" ++ show k ++ "...")
                    writeCircuit ("Examples/prio_init0_" ++ sn ++ "_" ++ sk ++ ".aig")
                                 (circuit (prio_props (Just False) n k))
                    writeCircuit ("Examples/prio_initX_" ++ sn ++ "_" ++ sk ++ ".aig")
                                 (circuit (prio_props Nothing n k))
               | n <- [4,5,6,7,8,16,32,64]
               , let sn | n < 10    = "0" ++ show n
                        | otherwise = show n
               , k <- [1..6]
               , let sk | k < 10    = "0" ++ show k
                        | otherwise = show k
               ]

--------------------------------------------------------------------------------

