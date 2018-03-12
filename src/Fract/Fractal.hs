module Fract.Fractal
    ( calcSquares
    ) where

import Control.Parallel.Strategies (parListChunk, rseq, using)

import Fract.Complex               ( Complex (C) )
import Fract.State                 ( View (View)
                                   , vOffset
                                   , vPosition)

import Fract.Types                 ( Coord(..)
                                   , Size(..)
                                   , ColorT(..)
                                   , Square(..)
                                   , Iter(..))

data IterProfile = IterProfile
    {-# UNPACK #-} !Complex  -- z
    {-# UNPACK #-} !Complex  -- c
    {-# UNPACK #-} !Iter     -- maxIter
    {-# UNPACK #-} !Double   -- bailout

data DemProfile = DemProfile
    {-# UNPACK #-} !Complex  -- dz
    {-# UNPACK #-} !Complex  -- z2
    {-# UNPACK #-} !Double   -- nz
    {-# UNPACK #-} !Double   -- nzp

dstep :: Iter -> IterProfile -> DemProfile -> (DemProfile, Iter)
dstep
    i
    (IterProfile (C x y) (C a b) maxIter bailout)
    d@(DemProfile (C dx dy) (C x2 y2) nz nzp)
    | nzp > 1e40 || nz > bailout || i > maxIter = (d, i)
    | otherwise =
        dstep
            (succ i)
            (IterProfile (C x' y') (C a b) maxIter bailout)
            (DemProfile (C dx' dy') (C x2' y2') nz' nzp')
    where
        -- Calculate the first derivative
        dx' = 2 * (x*dx - y*dy) + 1.0
        dy' = 2 * (x*dy + y*dx)

        -- z = z*z + c
        x' = x2 - y2 + a
        y' = 2*x*y + b

        -- abs
        x2' = x'*x'
        y2' = y'*y'
        nz' = x2'+y2'
        nzp' = dx'*dx' + dy'*dy'


newtype Distance = D Int deriving (Eq)

dist :: IterProfile -> Double
dist it@(IterProfile _ _ maxIter _)
    | i >= maxIter = 0
    | nzp < nz = 1
    | d <= 1.0 = d ** 0.25
    | nz > escapeRadius = 1
    | otherwise = 0
    where
        (DemProfile _ _ nz nzp, i) = dstep (Iter 0) it (DemProfile (C 0 0) (C 0 0) 0 0)
        d = (4 * sqrt (nz/nzp)*log nz) ** 0.25

colorD :: Double -> ColorT
colorD d = ColorT f f f
    where f | d < 0 = 0
            | d > 1 = 1
            | otherwise = realToFrac d

calcC :: View -> Size -> Coord -> Complex
calcC View { vPosition = C pre pim
           , vOffset   = C ore oim } (Size mx my) (Coord x y)
    = C (go pre ore mx x) (go pim oim my y)
    where
        go pos offset m n = i + step * fromIntegral n
            where
                i = pos - offset * 0.5
                step = offset / fromIntegral m

escapeRadius :: Double
escapeRadius = 33 * 33

calcD :: View -> Size -> Iter -> Coord -> Double
calcD v m maxIter sz
    = dist (IterProfile (C 0 0) (calcC v m sz) maxIter escapeRadius)

calcSquares :: View -> Size -> Iter -> [Square] -> [(Square, ColorT)]
calcSquares v m@(Size mx my) maxIter ss = zip ss ms
    where ms = map fn ss `using` parListChunk nChunks rseq
          fn = colorD . calcD v m maxIter . (\(Square sz _) -> sz)
          nChunks = maximum [mx,my] `div` 8
