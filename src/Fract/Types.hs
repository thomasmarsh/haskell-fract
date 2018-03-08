module Fract.Types
    ( Size(..)
    , Coord(..)
    , Square(..)
    , ColorT(..)
    ) where

data Coord  = Coord  Int Int           deriving (Show)
data Size   = Size   Int Int           deriving (Show)
data Square = Square Coord Int         deriving (Show)
data ColorT = ColorT Float Float Float deriving (Show)
