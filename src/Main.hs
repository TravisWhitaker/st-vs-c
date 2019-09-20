{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , NoImplicitPrelude
           , ViewPatterns
           #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe

import Criterion.Main

import Data.Array.MArray
import Data.Array.ST

import Data.Vector.Unboxed.Mutable

import Data.Word

import Foreign.Ptr

import Prelude hiding (read, length)

foreign import ccall unsafe call_overhead :: Word64 -> Word64

foreign import ccall safe "call_overhead" safe_call_overhead :: Word64 -> Word64

foreign import ccall unsafe cfree :: Ptr a -> IO ()

foreign import ccall unsafe cMallocAndPopWord :: Word64 -> IO (Ptr Word64)

foreign import ccall unsafe cMallocAndPopDouble :: Word64 -> IO (Ptr Double)

foreign import ccall unsafe cMapTimes2Word :: Ptr Word64 -> Word64 -> IO ()

foreign import ccall unsafe cMapTimes2Double :: Ptr Double -> Word64 -> IO ()

foreign import ccall unsafe cMapExpDouble :: Ptr Double -> Word64 -> IO ()

foreign import ccall unsafe cSwapInPlace :: Ptr Word64 ->  Word64 -> IO ()

type STWordArr s = STUArray s Word64 Word64

type STDoubleArr s = STUArray s Word64 Double

stMallocAndPopWord :: Word64 -> ST s (STWordArr s)
stMallocAndPopWord l =
    let go a !i | i == l    = pure ()
                | otherwise = do writeArray a i i
                                 go a (i + 1)
    in do
        a <- newArray_ (0, l - 1)
        go a 0
        pure a

stMallocAndPopDouble :: Word64 -> ST s (STDoubleArr s)
stMallocAndPopDouble l =
    let go a !i | i == l    = pure ()
                | otherwise = do writeArray a i (fromIntegral i / 10000)
                                 go a (i + 1)
    in do
        a <- newArray_ (0, l - 1)
        go a 0
        pure a

stMapTimes2Word :: STWordArr s -> ST s ()
stMapTimes2Word a =
    let go a !i !e
            | i > e     = pure ()
            | otherwise = do x <- readArray a i
                             writeArray a i (x * 2)
                             go a (i + 1) e
    in do
        (b, e) <- getBounds a
        go a b e

stMapTimes2Double :: STDoubleArr s -> ST s ()
stMapTimes2Double a =
    let go a !i !e
            | i > e     = pure ()
            | otherwise = do x <- readArray a i
                             writeArray a i (x * 2)
                             go a (i + 1) e
    in do
        (b, e) <- getBounds a
        go a b e

stMapExpDouble :: STDoubleArr s -> ST s ()
stMapExpDouble a =
    let go a !i !e
            | i > e     = pure ()
            | otherwise = do x <- readArray a i
                             writeArray a i (exp x)
                             go a (i + 1) e
    in do
        (b, e) <- getBounds a
        go a b e

stSwapInPlace :: STWordArr s -> ST s ()
stSwapInPlace a =
    let go a !i !e !l
            | i < (l `div` 2) = do
                t <- readArray a i
                x <- readArray a e
                writeArray a i x
                writeArray a e t
                go a (i + 1) (e - 1) l
            | otherwise = pure ()
    in do
        (b, e) <- getBounds a
        go a b e (e+1)

type VecWordArr s = STVector s Word64

type VecDoubleArr s = STVector s Double

vecMallocAndPopWord :: Word64 -> ST s (VecWordArr s)
vecMallocAndPopWord (fromIntegral -> l) =
    let go a !i | i == l = pure ()
                | otherwise = do write a i (fromIntegral i)
                                 go a (i + 1)
    in do
        a <- unsafeNew l
        go a 0
        pure a

vecMallocAndPopDouble :: Word64 -> ST s (VecDoubleArr s)
vecMallocAndPopDouble (fromIntegral -> l) =
    let go a !i | i == l = pure ()
                | otherwise = do write a i (fromIntegral i / 10000)
                                 go a (i + 1)
    in do
        a <- unsafeNew l
        go a 0
        pure a

vecMapTimes2Word :: VecWordArr s -> ST s ()
vecMapTimes2Word a =
    let go a !i !e
            | i > e     = pure ()
            | otherwise = do x <- read a i
                             write a i (x * 2)
                             go a (i + 1) e
    in go a 0 (length a)

vecMapTimes2Double :: VecDoubleArr s -> ST s ()
vecMapTimes2Double a =
    let go a !i !e
            | i > e     = pure ()
            | otherwise = do x <- read a i
                             write a i (x * 2)
                             go a (i + 1) e
    in go a 0 (length a)

vecMapExpDouble :: VecDoubleArr s -> ST s ()
vecMapExpDouble a =
    let go a !i !e
            | i > e     = pure ()
            | otherwise = do x <- read a i
                             write a i (exp x)
                             go a (i + 1) e
    in go a 0 (length a)

vecSwapInPlace :: VecWordArr s -> ST s ()
vecSwapInPlace a =
    let go a !i !e !l
            | i < (l `div` 2) = do
                t <- read a i
                x <- read a e
                write a i x
                write a e t
                go a (i + 1) (e - 1) l
            | otherwise = pure ()
        len = length a
    in do
        go a 0 (len - 1) len

benchEnum :: (Word64 -> Benchmarkable) -> Word64 -> Benchmark
benchEnum f n = bench (show n) (f n)

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "C" [ --bench "call_overhead" (whnf call_overhead 0)
                   --bench "safe_call_overhead" (whnf safe_call_overhead 0)
                   bgroup "mallocPopWord"
                          (map (benchEnum (whnfIO . cMallocAndPopWord)
                               ) [1000, 2000, 3000]
                          )
                 , bgroup "mallocPopDouble"
                          (map (benchEnum (whnfIO . cMallocAndPopDouble)
                               ) [1000, 2000 .. 3000]
                          )
                 ]
    , bgroup "STA" [ bgroup "mallocPopWord"
                            (map (benchEnum (\n -> whnf (\x -> runST (stMallocAndPopWord x *> pure ())) n)
                                 ) [1000, 2000 .. 3000]
                            )
                   , bgroup "mallocPopWord(unsafeSTToIO)"
                            (map (benchEnum (whnfIO . unsafeSTToIO . stMallocAndPopWord)
                                 ) [1000, 2000 .. 3000]
                            )
                   --, bgroup "mallocPopDouble"
                   --         (map (benchEnum (whnf (runST . void . stMallocAndPopDouble))
                   --              ) [1000, 2000 .. 3000]
                   --         )
                   ]
--    , bgroup "Vec" [ bgroup "mallocPopWord"
--                            (map (benchEnum (whnf (runST . void . vecMallocAndPopWord))
--                                 ) [1000, 2000 .. 3000]
--                            )
--                   , bgroup "mallocPopDouble"
--                            (map (benchEnum (whnf (runST . void . vecMallocAndPopDouble))
--                                 ) [1000, 2000 .. 3000]
--                            )
--                   ]
    ]

main :: IO ()
main = defaultMain benchmarks
