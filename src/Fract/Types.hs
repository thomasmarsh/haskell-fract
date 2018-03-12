{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fract.Types
    ( ColorT(..)
    , Coord(..)
    , Iter(..)
    , Size(..)
    , Square(..)
    ) where

newtype Iter = Iter Int                 deriving (Eq, Show, Enum, Ord)

data ColorT  = ColorT Float Float Float deriving (Show)
data Coord   = Coord Int Int            deriving (Show)
data Size    = Size Int Int             deriving (Show)
data Square  = Square Coord Int         deriving (Show)
