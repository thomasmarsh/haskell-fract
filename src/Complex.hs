module Complex
    ( Complex(..)
    , real
    , im
    , mag2
    , magnitude
    ) where

data Complex = C {-# UNPACK #-} !Double {-# UNPACK #-} !Double
-- data Complex = C Double Double
    deriving (Eq, Show)

instance Num Complex where
    fromInteger n = C (fromIntegral n) 0.0
    (C x y) * (C z t) = C (z * x - y * t) (y * z + x * t)
    (C x y) + (C z t) = C (x + z) (y + t)
    abs (C x y) = C (sqrt (x * x + y * y)) 0.0
    signum (C x y) = C (signum x) 0.0
    negate (C x y) = C (-x) (-y)

real :: Complex -> Double
real (C x y) = x

im :: Complex -> Double
im (C x y) = y

mag2 :: Complex -> Double
mag2 (C x y) = x * x + y * y

magnitude :: Complex -> Double
magnitude = sqrt . mag2
