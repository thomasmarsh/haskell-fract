module Fract.Fractal
    ( calcSquares
    ) where

import Control.Parallel.Strategies (parListChunk, rseq, using)

import Fract.Complex               ( Complex (C)
                                   , mag2
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

calc :: Complex -> Int -> IterPoint
calc c maxIter = go 0 c 0
  where
    go i z dz
        | i >= maxIter || nz > 4 = IterPoint de i
        | otherwise = go (i + 1) (z * z + c) dz'
      where
        nz = mag2 z
        dz' = C 2.0 0.0 * z * dz + C 1.0 0.0
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
