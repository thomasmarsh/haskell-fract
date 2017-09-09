module Fract.Fractal
    ( calcSquares
    ) where

import Control.Parallel.Strategies (parListChunk, rseq, using)

import Fract.Complex               (Complex (C)
                                   , im
                                   , real
                                   , mag2
                                   , magnitude)

type ColorT = (Float, Float, Float)
type Square = ((Int, Int), Int)

data Preset
    = Top
    | Detail

preset :: Preset
preset = Detail

calc :: Complex -> Complex -> Int -> (Double, Int)
calc z c maxIter = go 0 z 0
  where
    go i z dz
        | i >= maxIter || nz > 4 = (de, i)
        | otherwise = go (i + 1) (z * z + c) dz'
      where
        nz = mag2 z
        dz' = (C 2.0 0.0) * z * dz + (C 1.0 0.0)
        de = nz * log nz / magnitude dz

bailout :: Complex -> Bool
bailout z =
    (q * (q + tx)) < (0.25 * y * y) || ((x + 1) * (x + 1) + y * y < 0.0625)
  where
    x = real z
    y = im z
    tx = x - 0.25
    q = tx * tx + y * y

escapes :: Int -> Complex -> Bool
escapes maxIter z = not (bailout z || i >= maxIter)
  where
    (_, i) = calc z z maxIter

distance :: Int -> Complex -> Double
distance mxIter z
    | bailout z = 0.0
    | otherwise = nz
  where
    (nz, _) = calc z z mxIter

data View = View
    { vpx :: Double
    , vpy :: Double
    , vox :: Double
    , voy :: Double
    , vcutoff :: Double }

view :: View
view =
    case preset of
        Top ->
            View
            { vpx = -0.8
            , vpy = 0.0
            , vox = 3.0
            , voy = 3.0
            , vcutoff = 0.001 }
        Detail ->
            View
            { vpx = -0.751878166
            , vpy = -0.034018858
            , vox = 0.00083737688
            , voy = 0.00083737688
            , vcutoff = 0.00000005 }

toScreen :: Int -> Int -> Float
toScreen n m = fromIntegral n / (fromIntegral m * 0.5) - 1.0

calcZ :: View -> (Int, Int) -> Int -> Int -> Complex
calcZ View {vpx = px, vpy = py, vox = ox, voy = oy} (mx, my) x y =
    C (go px ox mx x) (go py oy my y)
  where
    go p o m n = i + s * fromIntegral n
      where
        i = p - o * 0.5
        s = o / fromIntegral m

calcZ' :: (Int, Int) -> Int -> Int -> Complex
calcZ' = calcZ view

isInSet :: (Int, Int) -> Int -> (Int, Int) -> Bool
isInSet m maxIter (x, y) = distance maxIter (calcZ' m x y) > vcutoff view

colorT :: Bool -> ColorT
colorT inSet
    | inSet = (1, 1, 1)
    | otherwise = (0, 0, 0)

calcSquares :: (Int, Int) -> Int -> [Square] -> [(Square, ColorT)]
calcSquares m@(mx, my) maxIter ss = zip ss ms
    where ms = map fn ss `using` parListChunk nChunks rseq
          fn = colorT . isInSet m maxIter . fst
          nChunks = maximum m `div` 8
