module Fract.Fractal
    ( calcSquares
    ) where

import Control.Parallel.Strategies (parListChunk, rseq, using)

import Fract.Complex               ( Complex (C)
                                   , magnitude)
import Fract.State                 ( View (View)
                                   , vOffset
                                   , vPosition
                                   , vCutoff)

import Fract.Types                 ( Coord(..)
                                   , Size(..)
                                   , ColorT(..)
                                   , Square(..))

data IterPoint = IterPoint Double Int deriving (Show)

data MCalc = MCalc {-# UNPACK #-} !Complex {-# UNPACK #-} !Complex deriving (Show)

-- Abstract out straight squaring so a more optimal function can
-- replace the multiplication.
square :: Num a => a -> a
square x = x*x

-- Provides basic `z = z * c` computation step minimizing multiplies.
-- This version is efficient for native precision doubles, performing
-- three multiplies per iteration.
fastStep :: MCalc -> Complex -> MCalc
fastStep (MCalc (C zr zi) (C zrsqr zisqr)) (C cr ci)
    = MCalc (C zr' zi') (C zrsqr' zisqr')
    where
        tmp    = zr * zi
        zr'    = zrsqr - zisqr + cr
        zi'    = tmp + tmp + ci
        zrsqr' = square zr'
        zisqr' = square zi'

-- This version should be used if Double is replaced with a high precision
-- real number type. At low precision this is inefficent because of an extra
-- subtraction, but we should see a speedup as the precision increases.
_fastStepH :: MCalc -> Complex -> MCalc
_fastStepH (MCalc (C zr zi) (C zrsqr zisqr)) (C cr ci)
    = MCalc (C zr' zi') (C zrsqr' zisqr')
    where
        zr'    = zrsqr - zisqr + cr
        zi'    = square (zr + zi) - zrsqr - zisqr + ci
        zrsqr' = square zr'
        zisqr' = square zi'

calc :: Complex -> Int -> IterPoint
calc c maxIter = go 0 (MCalc (C 0 0) (C 0 0)) 0
  where
    go i z@(MCalc (C zr zi) (C zrsqr zisqr)) dz
        | i >= maxIter || nz > 4 = IterPoint de i
        | otherwise = go (i + 1) (fastStep z c) dz'
      where
        nz = zrsqr + zisqr
        dz' = C (zr+zr) (zi+zi) * dz + C 1.0 0.0
        de = nz * log nz / magnitude dz

bailout :: Complex -> Bool
bailout (C re im) =
    (q * (q + tx)) < (0.25 * im * im) || ((re + 1) * (re + 1) + im * im < 0.0625)
  where
    tx = re - 0.25
    q = tx * tx + im * im

_escapes :: Int -> Complex -> Bool
_escapes maxIter z = not (bailout z || i >= maxIter)
  where
    (IterPoint _ i) = calc z maxIter

distance :: Int -> Complex -> Double
distance mxIter z
    | bailout z = 0.0
    | otherwise = nz
  where
    (IterPoint nz _) = calc z mxIter

calcZ :: View -> Size -> Coord -> Complex
calcZ View { vPosition = C pre pim
           , vOffset   = C ore oim } (Size mx my) (Coord x y) =
    C (go pre ore mx x) (go pim oim my y)
  where
    go pos offset m n = i + step * fromIntegral n
      where
        i = pos - offset * 0.5
        step = offset / fromIntegral m

isInSet :: View -> Size -> Int -> Coord -> Bool
isInSet v m maxIter z = distance maxIter (calcZ v m z) > vCutoff v

colorT :: Bool -> ColorT
colorT inSet
    | inSet = ColorT 0.5 0.7 1
    | otherwise = ColorT 0 0 0

calcSquares :: View -> Size -> Int -> [Square] -> [(Square, ColorT)]
calcSquares v m@(Size mx my) maxIter ss = zip ss ms
    where ms = map fn ss `using` parListChunk nChunks rseq
          fn = colorT . isInSet v m maxIter . (\(Square sz _) -> sz)
          nChunks = maximum [mx,my] `div` 8
