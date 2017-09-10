module Fract.Complex
    ( Complex(..)
    , mag2
    , magnitude
    ) where

data Complex = C {-# UNPACK #-} !Double {-# UNPACK #-} !Double
    deriving (Eq, Show)

instance Num Complex where
    fromInteger n = C (fromIntegral n) 0.0
    (C a b) * (C u v) = C (a*u - b*v) (b*u + a*v)
    (C a b) + (C u v) = C (a+u) (b+v)
    abs z = C (magnitude z) 0.0
    signum (C r _) = C (signum r) 0.0
    negate (C r i) = C (-r) (-i)

mag2 :: Complex -> Double
mag2 (C r i) = r*r + i*i

magnitude :: Complex -> Double
magnitude = sqrt . mag2
