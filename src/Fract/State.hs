module Fract.State
    ( View (..)
    , view
    , AppState
    , StateChange (..)
    , Preset (..)
    , hasRenderData
    , isShutdown
    , initState
    , stView
    , stMaxIter
    ) where

import Fract.Complex (Complex (C))
import Fract.Types   (ColorT(..), Square(..))

data Preset
    = Top
    | Detail
    deriving (Enum, Bounded)

data View = View
    { vPosition :: Complex
    , vOffset :: Complex
    , vCutoff :: Double    -- TODO: determine based on pixel size
    , vMaxIter :: Int
    }

view :: Preset -> View
view preset =
    case preset of
        Top ->
            View
            { vPosition = C (-0.8) 0.0
            , vOffset   = C 3.0 3.0
            , vCutoff   = 0.001
            , vMaxIter  = 500
            }
        Detail ->
            View
            { vPosition = C (-0.751878166) (-0.034018858)
            , vOffset   = C 0.00083737688 0.00083737688
            , vCutoff   = 0.00000005
            , vMaxIter  = 7000
            }

data StateChange
    = NewRenderData
    | Quit
    deriving (Show)

data AppState = AppState
    { stView :: View
    , isShutdown :: Bool -- is the app shutting down
    , hasRenderData :: Bool
    , renderBuf :: [[(Square, ColorT)]]
    , stMaxIter :: Int
    }

initState :: AppState
initState = AppState
    { stView = view Top
    , isShutdown = False
    , hasRenderData = False
    , renderBuf = []
    , stMaxIter = vMaxIter (view Top)
    }
