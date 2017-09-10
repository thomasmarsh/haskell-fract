module Fract.State
    ( View (..)
    , view
    , AppState
    , StateChange (..)
    , Preset (..)
    , isShutdown
    , initState
    , stView
    ) where

import Fract.Complex (Complex (C))

data Preset
    = Top
    | Detail
    deriving (Enum, Bounded)

data View = View
    { vPosition :: Complex
    , vOffset :: Complex
    , vCutoff :: Double    -- TODO: determine based on pixel size
    }

view :: Preset -> View
view preset =
    case preset of
        Top ->
            View
            { vPosition = C (-0.8) 0.0
            , vOffset   = C 3.0 3.0
            , vCutoff   = 0.001 }
        Detail ->
            View
            { vPosition = C (-0.751878166) (-0.034018858)
            , vOffset   = C 0.00083737688 0.00083737688
            , vCutoff   = 0.00000005 }

data StateChange
    = Quit
    deriving (Show)

data AppState = AppState
    { stView :: View
    , isShutdown :: Bool -- is the app shutting down
    }

initState :: AppState
initState = AppState
    { stView = view Detail
    , isShutdown = False
    }
