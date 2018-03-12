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
    {-# UNPACK #-} !Complex  -- cached x^2 and y^2 from previous iter
    {-# UNPACK #-} !Complex  -- c
    {-# UNPACK #-} !Iter     -- maxIter
    {-# UNPACK #-} !Double   -- bailout

data DemProfile = DemProfile
    {-# UNPACK #-} !Complex  -- dz
    {-# UNPACK #-} !Double   -- nz
    {-# UNPACK #-} !Double   -- nzp

dstep :: Iter -> IterProfile -> DemProfile -> (DemProfile, Iter)
dstep
    i
    (IterProfile (C x y) (C x2 y2) c@(C a b) maxIter bailout)
    d@(DemProfile (C dx dy) nz nzp)
    | nzp > 1e40 || nz > bailout || i > maxIter = (d, i)
    | otherwise =
        dstep
            (succ i)
            (IterProfile (C x' y') (C x2' y2') c maxIter bailout)
            (DemProfile (C dx' dy') nz' nzp')
    where
        -- Calculate the first derivative
        dx' = 2 * (x*dx - y*dy) + 1.0
        dy' = 2 * (x*dy + y*dx)

        -- z = z*z + c
        x' = x2 - y2 + a
        y' = 2*x*y + b

        -- cache x^2 and y^2 (to be used here and also next iter)
        x2' = x'*x'
        y2' = y'*y'

        -- abs
        nz' = x2'+y2'
        nzp' = dx'*dx' + dy'*dy'


newtype Distance = D Int deriving (Eq)

-- Return a distance from the set 0..1.
dist :: IterProfile -> Double
dist it@(IterProfile _ _ _ maxIter bailout)
    | i >= maxIter      = 0  -- not escaping
    | nz < bailout      = 0  -- not escaping
    | nzp < nz          = 1  -- escaping through 0
    | d <= 1.0          = d  -- boundary zone
    | nz > escapeRadius = 1  -- large distance outside set
    | otherwise         = 0  -- unreached
    where
        (DemProfile _ nz nzp, i) = dstep (Iter 0) it (DemProfile (C 0 0) 0 0)
        -- TODO: eliminate sqrt here
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
    = dist (IterProfile (C 0 0) (C 0 0) (calcC v m sz) maxIter escapeRadius)

calcSquares :: View -> Size -> Iter -> [Square] -> [(Square, ColorT)]
calcSquares v m@(Size mx my) maxIter ss = zip ss ms
    where ms = map fn ss `using` parListChunk nChunks rseq
          fn = colorD . calcD v m maxIter . (\(Square sz _) -> sz)
          nChunks = maximum [mx,my] `div` 8
