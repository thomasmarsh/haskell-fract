module Fractal
    ( fractMain
    ) where

import Complex
import Control.Parallel.Strategies
import Data.Colour.RGBSpace.HSV
import Graphics.UI.GLUT

data Preset
    = Top
    | Detail

preset = Detail

calc :: Complex -> Complex -> Int -> (Double, Int)
calc z c maxIter = go 0 z 0
  where
    go i z dz
        | i >= maxIter || nz > 4 = (de, i)
        | otherwise = go (i + 1) (z * z + c) dz'
      where
        nz = mag2 z
        dz' = complex 2 0 * z * dz + complex 1 0
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
    (nz, i) = calc z z maxIter

distance :: Int -> Complex -> Double
distance maxIter z
    | bailout z = 0.0
    | otherwise = nz
  where
    (nz, _) = calc z z maxIter

data View = View
    { vpx :: Double
    , vpy :: Double
    , vox :: Double
    , voy :: Double
    , vcutoff :: Double }

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

mx = 1000 :: Int

my = 1000 :: Int

maxIter = 7000

toScreen :: Int -> Int -> GLfloat
toScreen n m = fromIntegral n / (fromIntegral m * 0.5) - 1.0

calcZ :: View -> Int -> Int -> Int -> Int -> Complex
calcZ View {vpx = px, vpy = py, vox = ox, voy = oy} mx my x y =
    complex (go px ox mx x) (go py oy my y)
  where
    go p o m n = i + s * fromIntegral n
      where
        i = p - o * 0.5
        s = o / fromIntegral m

calcZ' :: Int -> Int -> Complex
calcZ' = calcZ view mx my

points =
    [ (toScreen x mx, toScreen y my, 0 :: GLfloat)
    | x <- [0 .. mx]
    , y <- [0 .. my]
    , distance maxIter (calcZ' x y) > vcutoff view
    ]

points' =
    concat
        ([ [ (toScreen x mx, toScreen y my, 0 :: GLfloat)
           | y <- [0 .. my]
           , distance maxIter (calcZ' x y) > vcutoff view]
         | x <- [0 .. mx]] `using` parListChunk (my `div` 8) rseq)

fractMain :: IO ()
fractMain = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello World"
    windowSize $= Size (fromIntegral mx :: GLint) (fromIntegral my :: GLint)
    displayCallback $= display
    mainLoop

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    renderPrimitive Points $
        mapM_
            (\((x, y, z), c) -> do
                 let c' = fromIntegral (c `mod` 256) / 256
                 currentColor $= Color4 c' (1 - c') ((c' + 1) / 2) 1
                 vertex $ Vertex3 x y z)
            (zip points' [0 ..])
    flush
