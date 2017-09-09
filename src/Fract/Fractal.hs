module Fract.Fractal
    ( fractMain
    ) where

import           Control.Concurrent          (threadDelay)
import           Control.Monad               (when)
import           Control.Parallel.Strategies (parListChunk, rseq, using)
import           System.Exit                 (exitFailure, exitSuccess)
import           System.IO                   (hPutStrLn, stderr)
import qualified Graphics.Rendering.OpenGL   as GL
import qualified Graphics.UI.GLFW            as GLFW

import           Fract.Complex               (Complex (C)
                                             , im
                                             , real
                                             , mag2
                                             , magnitude)
import           Fract.Subdivide            (getLevels)

type ColorT = (GL.GLfloat, GL.GLfloat, GL.GLfloat)
type Square = ((Int, Int), Int)

data Preset
    = Top
    | Detail

preset :: Preset
preset = Detail

calc :: Complex -> Complex -> Int -> (Double, Int)
calc z c mxIter = go 0 z 0
  where
    go i z dz
        | i >= mxIter || nz > 4 = (de, i)
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
escapes mxIter z = not (bailout z || i >= maxIter)
  where
    (_, i) = calc z z mxIter

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

mx :: Int
mx = 1000

my :: Int
my = 1000

maxIter :: Int
maxIter = 7000

toScreen :: Int -> Int -> GL.GLfloat
toScreen n m = fromIntegral n / (fromIntegral m * 0.5) - 1.0

calcZ :: View -> Int -> Int -> Int -> Int -> Complex
calcZ View {vpx = px, vpy = py, vox = ox, voy = oy} mx my x y =
    C (go px ox mx x) (go py oy my y)
  where
    go p o m n = i + s * fromIntegral n
      where
        i = p - o * 0.5
        s = o / fromIntegral m

calcZ' :: Int -> Int -> Complex
calcZ' = calcZ view mx my

isInSet :: (Int, Int) -> Bool
isInSet (x, y) = distance maxIter (calcZ' x y) > vcutoff view

colorT :: Bool -> ColorT
colorT inSet
    | inSet = (1, 1, 1)
    | otherwise = (0, 0, 0)

calcSquares :: [Square] -> [(Square, ColorT)]
calcSquares ss = zip ss ms
    where ms = map fn ss `using` parListChunk (my `div` 8) rseq
          fn = colorT . isInSet . fst

drawVertex :: (GL.GLfloat, GL.GLfloat) -> IO ()
drawVertex (x, y) = GL.vertex $ GL.Vertex3 x y 0

scaleFactor :: Float
scaleFactor = toScreen ((mx `div` 2) + 1) mx

drawSquare :: (Square, ColorT) -> IO ()
drawSquare (((x, y), w), (r, g, b)) = do
    let (x', y', w') = ( toScreen x mx
                       , toScreen y my
                       , fromIntegral w * scaleFactor)
    GL.color $ GL.Color3 r g b
    mapM_ drawVertex [(x',    y'),
                      (x'+w', y'),
                      (x'+w', y'+w'),
                      (x',    y'+w')]

drawSquares :: [(Square, ColorT)] -> IO ()
drawSquares = GL.renderPrimitive GL.Quads . mapM_ drawSquare

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

keyCallback :: GLFW.KeyCallback
keyCallback window key scancode action _
    = when ( key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
        GLFW.setWindowShouldClose window True

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x


fractMain :: IO ()
fractMain = do
    GLFW.setErrorCallback (Just errorCallback)
    successfulInit <- GLFW.init
    -- if init failed, we exit the program
    let ss = map calcSquares (getLevels (mx, my) 128)
    bool successfulInit exitFailure $ do
        mw <- GLFW.createWindow mx my "Mandelbrot" Nothing Nothing
        maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
            GLFW.makeContextCurrent mw
            GLFW.setKeyCallback window (Just keyCallback)
            mainLoop ss window
            GLFW.destroyWindow window
            GLFW.terminate
            exitSuccess

mainLoop :: [[(Square, ColorT)]] -> GLFW.Window -> IO ()
mainLoop ss w = do
    GL.clear [GL.ColorBuffer]
    mapM_ drawSquares ss
    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop ss w
