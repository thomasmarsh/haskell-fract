module Fract.State
    ( View (..)
    , view
    ) where

import Fract.Complex (Complex (C))

data Preset
    = Top
    | Detail
    deriving (Enum, Bounded)

preset :: Preset
preset = Detail

data View = View
    { vPosition :: Complex
    , vOffset :: Complex
    , vCutoff :: Double    -- TODO: determine based on pixel size
    }

view :: View
view =
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
