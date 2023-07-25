{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE FlexibleContexts, BangPatterns #-}

import Data.Int (Int64, Int32)
import Data.Bits (Bits(shiftR), (.|.))
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (STUArray, MArray(unsafeNewArray_), newArray,
                        unsafeAt, unsafeFreezeSTUArray, unsafeRead, unsafeWrite)

import Debug.Trace

primeCount :: Int64 -> Int64
primeCount n =
  if n < 9 then (if n < 2 then 0 else (n + 1) `div` 2) else
  let
    {-# INLINE divide #-}
    divide :: Int64 -> Int64 -> Int
    divide nm d = truncate $ (fromIntegral nm :: Double) / fromIntegral d
    {-# INLINE half #-}
    half :: Int -> Int
    half x = (x - 1) `shiftR` 1
    -- the mapping scheme is probably x -> (x - 1) / 2
    rtlmt = floor $ sqrt (fromIntegral n :: Double) 
    mxndx = (rtlmt - 1) `div` 2
    (npc, ns, smalls, roughs, larges) = runST $ do
      mss <- unsafeNewArray_ (0, mxndx) :: ST s (STUArray s Int Int32)
      forM_ [ 0 .. mxndx ] $ \ i -> unsafeWrite mss i (fromIntegral i) -- mss[i] = i
      
      mrs <- unsafeNewArray_ (0, mxndx) :: ST s (STUArray s Int Int32) -- mrs[i] = i * 2 + 1
      forM_ [ 0 .. mxndx ] $ \ i -> unsafeWrite mrs i (fromIntegral i * 2 + 1)
      
      mls <- unsafeNewArray_ (0, mxndx) :: ST s (STUArray s Int Int64)
      forM_ [ 0 .. mxndx ] $ \ i ->
        let d = fromIntegral (i + i + 1)
        in unsafeWrite mls i (fromIntegral (divide n d - 1) `div` 2)
      -- mls[i] = (n / (2i+1) - 1) / 2 (the large bound)
      skips <- newArray (0, mxndx) False :: ST s (STUArray s Int Bool)
      let loop i !pc !cs =
            let sqri = (i + i) * (i + 1) in
            if sqri > mxndx then do
              fss <- unsafeFreezeSTUArray mss
              frs <- unsafeFreezeSTUArray mrs
              fls <- unsafeFreezeSTUArray mls
              return (pc, cs + 1, fss, frs, fls)
            else do
              v <- unsafeRead skips i
              -- if skips i then continue to loop with i + 1
              if v then loop (i + 1) pc cs else do
                -- handle i now, we can skip i if we encounter it later
                unsafeWrite skips i True
                let bp = i + i + 1 -- original prime?
                    -- skip c, c + bp, ...
                    -- I guess that c is the boundary
                    cull c = if c > mxndx then return () else do
                               unsafeWrite skips c True
                               cull (c + bp)
                    part k !s =
                      -- s is ori. Starts at 0
                      if k > cs then return (s - 1) else do
                        -- read the rough value
                        -- p = k-roughs
                        p <- unsafeRead mrs k
                        t <- unsafeRead skips ((fromIntegral p - 1) `shiftR` 1)
                        -- if p can be skipped, we continue
                        if t then part (k + 1) s else do
                          -- read the large value
                          -- mls[k]
                          olv <- unsafeRead mls k
                          -- d = the prime * the rough value
                          -- odd product of some combination of odd primes
                          let d = fromIntegral p * fromIntegral bp
                          -- adjacent value?
                          adjv <- -- return olv
                                  if d <= fromIntegral rtlmt then do
                                    -- if d is less than the root limit
                                    sv <- unsafeRead mss (fromIntegral d `shiftR` 1)
                                    -- read the short value, and then a large value?
                                    -- pc is the base prime??
                                    unsafeRead mls (fromIntegral sv - pc)
                                  else do
                                    -- read small value of the "compliment"
                                    sv <- unsafeRead mss (half (divide n d))
                                    return (fromIntegral sv)
                          -- write into mls with some new value
                          unsafeWrite mls s (olv - adjv + fromIntegral pc)
                          -- also write into mrs with some value
                          -- equivalent to roughs[ori] = q
                          unsafeWrite mrs s p
                          -- continue the recursion
                          part (k + 1) (s + 1)
                    k0 = ((rtlmt `div` bp) - 1) .|. 1
                    adjc !j k =
                      -- j: loop index of the inner loop. starts at mxndx
                      -- loop down
                      if k < bp then return () else do
                        c <- unsafeRead mss (k `shiftR` 1)
                        let ac = c - fromIntegral pc -- equivalent to c
                            e = (k * bp) `shiftR` 1
                            -- while m >= e do
                            -- next iteration of adjc (the outer loop)
                            adj m = if m < e then adjc m (k - 2)
                                    -- next iteration of the inner loop 
                                    else do ov <- unsafeRead mss m
                                            unsafeWrite mss m (ov - ac)
                                            -- mss[m] -= ac
                                            adj (m - 1)
                        adj j
                cull sqri
                his <- part 0 0
                adjc mxndx k0
                -- let !printMss = traceShowId mss
                -- let !printMrs = traceShowId mrs
                -- let !printMls = traceShowId mls
                -- cs is the rough index?
                loop (i + 1) (pc + 1) his
      loop 1 0 mxndx
    ans0 = unsafeAt larges 0 + fromIntegral ((ns + 2 * (npc - 1)) * (ns - 1) `div` 2)
    accum j !ans =
      if j >= ns then ans else
      let q = fromIntegral (unsafeAt roughs j)
          m = n `div` q
          e = fromIntegral (unsafeAt smalls (half (fromIntegral (m `div` q)))) - npc
          comp k !ac =
            if k > e then ac else
            let p = fromIntegral (unsafeAt roughs k)
                ci = half (fromIntegral (divide m p))
            in comp (k + 1) (ac + fromIntegral (unsafeAt smalls ci))
      in if e <= j then ans else
         accum (j + 1) (comp (j + 1) ans - fromIntegral ((e - j) * (npc + j - 1)))
  in accum 1 (ans0 - sum [ unsafeAt larges i | i <- [ 1 .. ns - 1 ] ]) + 1

main :: IO ()
main = forM_ [0 .. 8] $ \i -> print $ primeCount (10^ (i :: Int))

