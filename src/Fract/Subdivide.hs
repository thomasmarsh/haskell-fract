module Fract.Subdivide
    ( getLevels
    , isPow2
    , Square
    ) where

import Data.Bits   ((.&.))
import Fract.Types (Square(..), Coord(..), Size(..))

data Elt
    = Visible Square
    | Blank
    deriving (Show)

data Tree
    = Node Elt [Tree]
    deriving (Show)

isPow2 :: Int -> Bool
isPow2 n
    | n < 1 = False
    | otherwise = n .&. (n-1) == 0

onScreen :: Size -> Square -> Bool
onScreen (Size mx my) (Square (Coord x y) _) = x < mx && y < my

numPoints :: Size -> Int
numPoints (Size x y) = x * y

triad :: Size -> Square -> [Square]
triad m (Square (Coord x y) w)
    = filter (onScreen m) [Square (Coord (x+w) y)     w,
                           Square (Coord x     (y+w)) w,
                           Square (Coord (x+w) (y+w)) w]

subdivide :: Size -> Square -> [Tree]
subdivide m (Square (Coord x y) w)
    | w == 2 = map (visible []) tri
    | otherwise = blank : map (\z -> visible (subdivide m z) z) tri
    where v = w `div` 2
          sub = Square (Coord x y) v
          tri = triad m sub
          blank = Node Blank (subdivide m sub)
          visible xs z = Node (Visible z) xs

tree :: Size -> Square -> Tree
tree m z@(Square (Coord _ _) w)
    | w < 2 = error "cannot subdivide width less than 2"
    | otherwise = Node (Visible z) (subdivide m z)

count :: Int -> Int -> Int
count m w
    | (m `rem` w) == 0 = m `div` w
    | otherwise = (m `div` w)+1

chunks :: Size -> Int -> [Square]
chunks (Size mx my) w
    = [ Square (Coord (x*w) (y*w)) w
      | y <- [0..count (my-w) w]
      , x <- [0..count (mx-w) w]]

bestPow2 :: Int -> Int
bestPow2 n = last $ takeWhile (<= n) $ map (2^) [(0::Int)..]

forest :: Size -> Int -> Tree
forest m@(Size mx my) w
    | mx < 0 || my < 0 = error "dimensions must be positive"
    | mx < 2 || my < 2 = error "dimensions must be >2"
    | not (isPow2 w) = error ("w must be a power of 2 (value=" ++ show w ++ ")")
    | otherwise = Node Blank xs
    where xs = map (tree m) (chunks m w')
          w' | w > maximum [mx, my] = bestPow2 w
             | otherwise = w

levels :: Tree -> [[Square]]
levels (Node Blank bs) = [] : levels' bs
levels (Node (Visible a) bs) = [a] : levels' bs

levels' :: [Tree] -> [[Square]]
levels' = foldr (zipWith' (++) . levels) []

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ xs [] = xs
zipWith' _ [] xs = xs
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

squares :: Tree -> [Square]
squares = concat . levels

printElem :: Int -> String -> [Tree] -> IO ()
printElem n repr xs = do
    putStrLn $ replicate n '-' ++ "> " ++ repr
    mapM_ (printTree (n+1)) xs

printTree :: Int -> Tree -> IO ()
printTree n (Node (Visible z) xs) = printElem n (show z) xs
printTree n (Node Blank xs) = printElem n "<blank>" xs

getLevels :: Size -> Int -> [[Square]]
getLevels m n = filter (not . null) $ levels $ forest m n

_test :: IO ()
_test = do
    let m = Size 4 4
    let w = 128
    let r = forest m w
    let c = chunks m w
    putStrLn $ "chunks = " ++ show c
    mapM_ (\x ->
            putStrLn $ "level: " ++ show (length x) ++ ": " ++ show x) (levels r)
    let s = squares r
    let len = length s
    putStrLn $ "len = " ++ show len
    putStrLn $ "Length check: " ++ (if numPoints m == len
                                    then "OK"
                                    else "ERROR: expected=" ++ show (numPoints m)
                                         ++ " actual=" ++ show len)

    print $ getLevels (Size 1023 769) 128
    printTree 0 r
