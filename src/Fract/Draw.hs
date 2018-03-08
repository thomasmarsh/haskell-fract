module Fract.Draw
    ( fractMain
    ) where

import           Control.Concurrent.STM    ( TChan
                                           , atomically
                                           , isEmptyTChan
                                           , readTChan
                                           , newTChanIO
                                           , writeTChan
                                           )
import           Control.Monad             (when)
import           Control.Monad.Catch       (finally)
import           System.Exit               (exitFailure, exitSuccess)
import           System.IO                 ( hPutStrLn
                                           , hFlush
                                           , stderr
                                           , stdout
                                           )
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           Fract.Fractal             (calcSquares)
import           Fract.Subdivide           (getLevels)
import           Fract.Types               ( Coord(..)
                                           , Square(..)
                                           , ColorT(..)
                                           , Size(..))
import           Fract.State               ( AppState
                                           , StateChange (Quit, NewRenderData)
                                           , hasRenderData
                                           , initState
                                           , isShutdown
                                           , stMaxIter
                                           , stView)

toScreen :: Int -> Int -> GL.GLfloat
toScreen n m = fromIntegral n / (fromIntegral m * 0.5) - 1.0

drawVertex :: (GL.GLfloat, GL.GLfloat) -> IO ()
drawVertex (x, y) = GL.vertex $ GL.Vertex3 x y 0

-- TODO: this is a quick hack - replace it.
scaleFactor :: Size -> Float
scaleFactor (Size mx _) = toScreen ((mx `div` 2) + 1) mx

drawSquare :: Size -> (Square, ColorT) -> IO ()
drawSquare m@(Size mx my) (Square (Coord x y) w, ColorT r g b) = do
    let (x', y', w') = ( toScreen x mx
                       , toScreen y my
                       , fromIntegral w * scaleFactor m)
    GL.color $ GL.Color3 r g b
    mapM_ drawVertex [(x',    y'),
                      (x'+w', y'),
                      (x'+w', y'+w'),
                      (x',    y'+w')]

drawSquares :: Size -> [(Square, ColorT)] -> IO ()
drawSquares m = GL.renderPrimitive GL.Quads . mapM_ (drawSquare m)

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

cbKey :: TChan StateChange -> GLFW.KeyCallback
cbKey chan _window key _scancode action _modifier
    = when ( key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
        atomically $ writeTChan chan Quit

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x

fractMain :: IO ()
fractMain = do
    let m = (1000, 1000)
    let screenSize = uncurry Size m

    GLFW.setErrorCallback (Just errorCallback)
    successfulInit <- GLFW.init

    -- if init failed, we exit the program
    bool successfulInit exitFailure $ do
        let app = initState
        let ss = map (calcSquares (stView app) screenSize (stMaxIter app)) (getLevels screenSize 128)
        chan <- newTChanIO :: IO (TChan StateChange)

        mw <- uncurry GLFW.createWindow m "Mandelbrot" Nothing Nothing
        maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
            GLFW.makeContextCurrent mw
            GLFW.setKeyCallback window (Just (cbKey chan))
            mainLoop app chan screenSize ss window `finally` 
                ( GLFW.destroyWindow window
                >> GLFW.terminate
                >> exitSuccess)

mainLoop :: AppState
         -> TChan StateChange
         -> Size
         -> [[(Square, ColorT)]]
         -> GLFW.Window
         -> IO ()
mainLoop app chan m ss w =
    if isShutdown app then
        return ()
    else do
        GL.clear [GL.ColorBuffer]
        mapM_ (drawSquares m) ss
        GLFW.swapBuffers w
        GLFW.pollEvents
        app' <- handleEvents chan app
        mainLoop app' chan m ss w

handleEvents :: TChan StateChange -> AppState -> IO AppState
handleEvents chan app = do
    emptyChan <- atomically $ isEmptyTChan chan
    if emptyChan then
        return app
    else do
        msg <- atomically $ readTChan chan
        print msg >> hFlush stdout
        handleEvents chan $ case msg of
            NewRenderData -> app { hasRenderData = True }
            Quit -> app { isShutdown = True }

