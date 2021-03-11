{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , MagicHash
           , NoImplicitPrelude
           #-}

module Main where

import Control.DeepSeq.Instances

import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe

import Criterion.Main

import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe

import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as V

import Data.Word

import Foreign.Ptr

import Prelude hiding (read, length)

foreign import ccall unsafe call_overhead :: Word64 -> Word64

foreign import ccall safe "call_overhead" safe_call_overhead :: Word64 -> Word64

foreign import ccall unsafe cfree :: Ptr a -> IO ()

foreign import ccall unsafe cAllocWord :: Word64 -> IO (Ptr Word64)

foreign import ccall unsafe cAllocDouble :: Word64 -> IO (Ptr Double)

foreign import ccall unsafe cPopWord :: Ptr Word64 -> Word64 -> IO ()

foreign import ccall unsafe cPopDouble :: Ptr Double -> Word64 -> IO ()

foreign import ccall unsafe cMapTimes2Word :: Ptr Word64 -> Word64 -> IO ()

foreign import ccall unsafe cMapTimes2Double :: Ptr Double -> Word64 -> IO ()

foreign import ccall unsafe cMapExpDouble :: Ptr Double -> Word64 -> IO ()

foreign import ccall unsafe cSwapInPlace :: Ptr Word64 ->  Word64 -> IO ()

type STWordArr s = STUArray s Word64 Word64

type STDoubleArr s = STUArray s Word64 Double

stAllocWord :: Word64 -> ST s (STWordArr s)
stAllocWord l = newArray_ (0, l - 1)

stAllocDouble :: Word64 -> ST s (STDoubleArr s)
stAllocDouble l = newArray_ (0, l - 1)

stPopWord :: STWordArr s -> ST s ()
stPopWord a =
    let go a !i !e
            | i > e     = pure ()
            | otherwise = do writeArray a i i
                             go a (i + 1) e
    in do
        (b, e) <- getBounds a
        go a b e

stPopDouble :: STDoubleArr s -> ST s ()
stPopDouble a =
    let go a !i !e
            | i > e     = pure ()
            | otherwise = do writeArray a i (fromIntegral i / 10000)
                             go a (i + 1) e
    in do
        (b, e) <- getBounds a
        go a b e

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

type VecWordArr s = V.STVector s Word64

type VecDoubleArr s = V.STVector s Double

vecAllocWord :: Word64 -> ST s (VecWordArr s)
vecAllocWord = V.unsafeNew . fromIntegral

vecAllocDouble :: Word64 -> ST s (VecDoubleArr s)
vecAllocDouble = V.unsafeNew . fromIntegral

vecPopWord :: VecWordArr s -> ST s ()
vecPopWord a =
    let go a !i !e
            | i == e    = pure ()
            | otherwise = do V.unsafeWrite a i (fromIntegral i)
                             go a (i + 1) e
    in go a 0 (V.length a)

vecPopDouble :: VecDoubleArr s -> ST s ()
vecPopDouble a =
    let go a i e
            | i == e    = pure ()
            | otherwise = do V.unsafeWrite a i (fromIntegral i / 10000)
                             go a (i + 1) e
    in go a 0 (V.length a)

vecMapTimes2Word :: VecWordArr s -> ST s ()
vecMapTimes2Word a =
    let go a !i !e
            | i > e     = pure ()
            | otherwise = do x <- V.unsafeRead a i
                             V.unsafeWrite a i (x * 2)
                             go a (i + 1) e
    in go a 0 (V.length a)

vecMapTimes2Double :: VecDoubleArr s -> ST s ()
vecMapTimes2Double a =
    let go a !i !e
            | i > e     = pure ()
            | otherwise = do x <- V.unsafeRead a i
                             V.unsafeWrite a i (x * 2)
                             go a (i + 1) e
    in go a 0 (V.length a)

vecMapExpDouble :: VecDoubleArr s -> ST s ()
vecMapExpDouble a =
    let go a !i !e
            | i > e     = pure ()
            | otherwise = do x <- V.unsafeRead a i
                             V.unsafeWrite a i (exp x)
                             go a (i + 1) e
    in go a 0 (V.length a)

vecSwapInPlace :: VecWordArr s -> ST s ()
vecSwapInPlace a =
    let go a !i !e !l
            | i < (l `div` 2) = do
                t <- V.unsafeRead a i
                x <- V.unsafeRead a e
                V.unsafeWrite a i x
                V.unsafeWrite a e t
                go a (i + 1) (e - 1) l
            | otherwise = pure ()
        len = V.length a
    in do
        go a 0 (len - 1) len

benchEnum :: (Word64 -> Benchmarkable) -> Word64 -> Benchmark
benchEnum f n = bench (show n) (f n)

sizes = [1000]

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "C" [ --bench "call_overhead" (whnf call_overhead 0)
                   --bench "safe_call_overhead" (whnf safe_call_overhead 0)
                 --  bgroup "allocWord"
                 --         (map (benchEnum (whnfIO . cAllocWord)
                 --              ) [1000]
                 --         )
                 --, bgroup "allocDouble"
                 --         (map (benchEnum (whnfIO . cAllocDouble)
                 --              ) [1000]
                 --         )
                   bgroup "popWord"
                          (map (\n -> env (cAllocWord n)
                                          (\ ~a -> benchEnum (\n -> whnfIO (cPopWord a n))
                                                             n
                                          )
                               ) sizes
                          )
                 , bgroup "popDouble"
                          (map (\n -> env (cAllocDouble n)
                                          (\ ~a -> benchEnum (\n -> whnfIO (cPopDouble a n))
                                                             n
                                          )
                               ) sizes
                          )
                 ]
    --, bgroup "STA" [ bgroup "allocWord"
    --                        (map (benchEnum (\n -> whnf (\x -> runST (stAllocWord x *> pure ())) n)
    --                             ) [1000]
    --                        )
    --               , bgroup "allocWord(unsafeSTToIO)"
    --                        (map (benchEnum (whnfIO . unsafeSTToIO . stAllocWord)
    --                             ) [1000]
    --                        )
    --               , bgroup "allocDouble"
    --                        (map (benchEnum (\n -> whnf (\x -> runST (stAllocDouble x *> pure ())) n)
    --                             ) [1000]
    --                        )
    --               , bgroup "popWord"
    --                        (map (\n -> env (pure (runST (stAllocWord n >>= unsafeFreeze) :: UArray Word64 Word64))
    --                                        (\ ~a -> benchEnum (\_ -> whnf (\x -> runST ((unsafeThaw x >>= stPopWord) *> pure ()))
    --                                                                       a
    --                                                           )
    --                                                           n
    --                                        )
    --                             ) [1000]
    --                        )
    --               , bgroup "popDouble"
    --                        (map (\n -> env (pure (runST (stAllocDouble n >>= unsafeFreeze) :: UArray Word64 Double))
    --                                        (\ ~a -> benchEnum (\_ -> whnf (\x -> runST ((unsafeThaw x >>= stPopDouble) *> pure ()))
    --                                                                       a
    --                                                           )
    --                                                           n
    --                                        )
    --                             ) [1000]
    --                        )
    --               --, bgroup "mallocPopDouble"
    --               --         (map (benchEnum (whnf (runST . void . stMallocAndPopDouble))
    --               --              ) [1000, 2000 .. 3000]
    --               --         )
    --               ]
    , bgroup "Vec" [
                   --  bgroup "allocWord"
                   --         (map (benchEnum (\n -> whnf (\x -> runST (vecAllocWord x *> pure ())) n)
                   --              ) [1000]
                   --         )
                   --, bgroup "allocDouble"
                   --         (map (benchEnum (\n -> whnf (\x -> runST (vecAllocWord x *> pure ())) n)
                   --              ) [1000]
                   --         )
                     bgroup "popWord"
                            (map (\n -> env (pure (runST (vecAllocWord n >>= VU.unsafeFreeze)))
                                            (\ ~a -> benchEnum (\_ -> whnf (\x -> runST ((VU.unsafeThaw x >>= vecPopWord) *> pure ()))
                                                                           a
                                                               )
                                                               n
                                            )
                                 ) sizes
                            )
                   , bgroup "popDouble"
                            (map (\n -> env (pure (runST (vecAllocDouble n >>= VU.unsafeFreeze)))
                                            (\ ~a -> benchEnum (\_ -> whnf (\x -> runST ((VU.unsafeThaw x >>= vecPopDouble) *> pure ()))
                                                                           a
                                                               )
                                                               n
                                            )
                                 ) sizes
                            )
                   ]
    ]

main :: IO ()
main = defaultMain benchmarks
