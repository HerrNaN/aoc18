module Day11 (day11a, day11b) where

import Data.List
import Data.Ord (comparing)
import Data.Function (on)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

type Pos  = (Int, Int)
data Cell = Cell {position :: Pos, power :: Int}
    deriving Show

getPower :: Pos -> Int -> Int
getPower (x,y) serial = hd - 5
    where rid = x + 10
          pws = (rid * y + serial) * rid
          hd  = (pws `div` 100) `mod` 10

grid :: [Pos]
grid = [(x,y) | y <- [1..300], x <- [1..300]]

fuelGrid :: [Pos] -> Int -> Seq Cell
fuelGrid ps s = S.fromList [Cell pos (getPower pos s) | pos <- ps]

findMaxSquare :: Int -> Seq Cell -> (Pos, Int)
findMaxSquare size cs = maximumBy (compare `on` snd) [ ((x,y), getPowerSum (x,y) size cs) | x <- [1..300-size], 
                                                                                            y <- [1..300-size] ]

getPowerSum :: Pos -> Int -> Seq Cell -> Int
getPowerSum (x,y) n cs = sum $ map power [ cs `S.index` (pos + y' * 300 + x') | x' <- [0..n-1],
                                                                                y' <- [0..n-1] ]
    where pos = (y - 1) * 300 + x - 1

day11a :: String -> Pos
day11a s = fst $ findMaxSquare 3 $ fuelGrid grid $ (read::String->Int) s

day11b :: String -> (Int, Int, Int)
day11b s = (x,y,s')
    where (s',((x,y),_)) = maximumBy (compare `on` (snd . snd)) maxSquares
          maxSquares = [(n, findMaxSquare  n $ fuelGrid grid input) | n <- [1..300]]
          input = (read::String->Int) s
