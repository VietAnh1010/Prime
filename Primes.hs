{-# OPTIONS_GHC -O1 #-}
{-# LANGUAGE BangPatterns #-}

import Data.Bits       ( shiftL, shiftR )
import Data.Array.Base ( IArray(unsafeAt)
                       , UArray(UArray)
                       , MArray(newArray, unsafeWrite) 
                       )
import Control.Monad   ( forM_ )
import Data.Array.ST   ( runSTUArray )

import Control.Monad
import Data.Array.Unboxed
import Data.Array.ST

import Debug.Trace

cSieveBufferLimit :: Int
cSieveBufferLimit = 2 ^ 20 - 1 -- CPU L2 cache in bits

primes :: () -> [Int]
primes () = 2 : _Y (listPagePrimes . makePages) where
  _Y g = g (_Y g) -- non-sharing multi-stage fixpoint combinator
  listPagePrimes pgs@(hpg@(UArray li _ len _) : tpgs) =
    let loop i
          | i >= len       = listPagePrimes tpgs
          | unsafeAt hpg i = loop (i + 1)
          | otherwise      = let ii = li + i
                                 bp = (ii `shiftL` 1) + 3
                             in bp `seq` bp : loop (i + 1)
    in loop 0
  -- mapping scheme: x -> (x - 3) / 2
  -- reverse mapping: limit -> (limit * 2) + 3
  makePage li bps = runSTUArray $ do
    let hi = li + cSieveBufferLimit
        bplmt = floor . sqrt . fromIntegral $ (hi `shiftL` 1) + 3
        getsp bp = let si = (bp ^ 2 - 3) `shiftR` 1 
                   in if si >= li 
                     then si - li
                     else 
                       let r = (li - si) `rem` bp
                       in if r == 0 
                         then 0 
                         else bp - r
    bcs <- newArray (li, hi) False
    let cbps = if li == 0 then [3, 5 .. bplmt] else takeWhile (<= bplmt) bps
    -- let !shouldTrace = if li == 0 then traceShowId cbps else []
    -- let cbps = takeWhile (<= bplmt) bps
    -- for all primes in cbps that is less then bplmt
    -- cbps is the prime stream
    forM_ cbps $ \bp ->
      -- for all composite numbers in the range
      -- set the mask to True (is composite)
      forM_ (let sp = getsp bp
             in [sp, sp + bp .. cSieveBufferLimit]) $ \bc ->
        unsafeWrite bcs bc True
    return bcs
  makePages bps = [makePage li bps | li <- [0, cSieveBufferLimit + 1 ..]]


sieveUA :: Int -> UArray Int Bool
sieveUA top = runSTUArray $ do
    let m = (top-1) `div` 2
        r = floor . sqrt $ fromIntegral top + 1
    sieve <- newArray (1,m) True      -- :: ST s (STUArray s Int Bool)
    forM_ [1..r `div` 2] $ \i -> do
      isPrime <- readArray sieve i
      when isPrime $ do               -- ((2*i+1)^2-1)`div`2 == 2*i*(i+1)
        forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j -> do
          writeArray sieve j False
    return sieve
 
primesToUA :: Int -> [Int]
primesToUA top = 2 : [i*2+1 | (i,True) <- assocs $ sieveUA top]

main :: IO ()
main = print $ last $ take (10 ^ (2 :: Int)) $ primes()
