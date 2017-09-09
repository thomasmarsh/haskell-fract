module Fract.Draw
    ( fractMain
    ) where

import           Control.Monad             (when)
import           System.Exit               (exitFailure, exitSuccess)
import           System.IO                 (hPutStrLn, stderr)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           Fract.Fractal             (calcSquares)
import           Fract.Subdivide           (getLevels)


type Square = ((Int, Int), Int)
type ColorT = (Float, Float, Float)

toScreen :: Int -> Int -> GL.GLfloat
toScreen n m = fromIntegral n / (fromIntegral m * 0.5) - 1.0

drawVertex :: (GL.GLfloat, GL.GLfloat) -> IO ()
drawVertex (x, y) = GL.vertex $ GL.Vertex3 x y 0

scaleFactor :: (Int, Int) -> Float
scaleFactor (mx, my) = toScreen ((mx `div` 2) + 1) mx

drawSquare :: (Int, Int) -> (Square, ColorT) -> IO ()
drawSquare m@(mx, my) (((x, y), w), (r, g, b)) = do
    let (x', y', w') = ( toScreen x mx
                       , toScreen y my
                       , fromIntegral w * scaleFactor m)
    GL.color $ GL.Color3 r g b
    mapM_ drawVertex [(x',    y'),
                      (x'+w', y'),
                      (x'+w', y'+w'),
                      (x',    y'+w')]

drawSquares :: (Int, Int) -> [(Square, ColorT)] -> IO ()
drawSquares m = GL.renderPrimitive GL.Quads . mapM_ (drawSquare m)

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
    let screenSize = (1000, 1000)
    let maxIter = 7000

    GLFW.setErrorCallback (Just errorCallback)
    successfulInit <- GLFW.init
    -- if init failed, we exit the program
    let ss = map (calcSquares screenSize maxIter) (getLevels screenSize 128)
    bool successfulInit exitFailure $ do
        mw <- (uncurry GLFW.createWindow screenSize) "Mandelbrot" Nothing Nothing
        maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
            GLFW.makeContextCurrent mw
            GLFW.setKeyCallback window (Just keyCallback)
            mainLoop screenSize ss window
            GLFW.destroyWindow window
            GLFW.terminate
            exitSuccess

mainLoop :: (Int, Int) -> [[(Square, ColorT)]] -> GLFW.Window -> IO ()
mainLoop m ss w = do
    GL.clear [GL.ColorBuffer]
    mapM_ (drawSquares m) ss
    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop m ss w