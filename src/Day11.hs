module Day11 where

import Data.List
import Data.Ord (comparing)
import Data.Function (on)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Map (Map)
import qualified Data.Map as Map
import Control.DeepSeq
import Data.Array (Array, (!))
import qualified Data.Array as A



type Serial = Int
type Pos  = (Int, Int)
{- data Cell = Cell {position :: Pos, power :: Int}
    deriving Show -}

getPower :: Int -> Int -> Serial -> Int
getPower i n = getPower' (indexToPos i n)

getPower' :: Pos -> Serial -> Int
getPower' (x,y) serial = hd - 5
    where rid = x + 10
          pws = (rid * y + serial) * rid
          hd  = (pws `div` 100) `mod` 10

{- grid :: Int -> Serial -> Matrix Int
grid n s = M.matrix n n $ \(r,c) -> getPower (c,r) s
-}

indexToPos :: Int -> Int -> Pos
indexToPos i n = (((i - 1) `mod` n) + 1, ((i - 1) `quot` n) + 1)

--             Index  Size   dx     dy
modifyIndex :: Int -> Int -> Int -> Int -> Int
modifyIndex i s 0  0  = i
modifyIndex i s dx 0  = i + dx
modifyIndex i s 0  dy = i + (s * dy)
modifyIndex i s dx dy = i + dx' + dy'
    where dx' = modifyIndex i s dx 0 - i
          dy' = modifyIndex i s 0 dy - i

grid :: Int -> Serial -> Array Int Int
grid n s = a
    where a = A.array (1, n^2) ((1, getPower 1 n s) : [(i, ppCell i s n a) | i <- [2..(n^2)]])
          
ppCell :: Int -> Int -> Int -> Array Int Int -> Int
ppCell i s n a | i <= n         = v + a ! (i-1)
               | i `mod` n == 1 = v + a ! (i-n)
               | otherwise      = v + a ! (i-1)
                                    + a ! (i-n)
                                    - a ! (i-n-1)
    where v = getPower i n s

findMaxSquare :: Int -> Int -> Array Int Int -> (Pos, Int)
findMaxSquare size n is = maximumBy (compare `on` snd) 
    [(indexToPos ix n, square ix n size is) | ix <- A.indices is, 
                                                 (ix-1) `mod` n + (size - 1) < n,
                                                 (ix-1) `quot` n + (size - 1) < n]
{- findMaxSquare :: Int -> Array Int Int -> (Pos, Int)
findMaxSquare size mx = maximumBy (compare `on` snd) [ ((r,c), getSum (r,c) size mx) | r <- [1..(M.nrows mx - size)], 
                                                                                       c <- [1..(M.ncols mx - size)] ] -}

{-| |---|---|---|---|
  | | r |   |   | p |
  | |---|---|---|---|
  | |   | i |   |   |
  | |---|---|---|---|
  | |   |   |   |   |
  | |---|---|---|---|
  | | q |   |   | i'|
  | |---|---|---|---|
  |
-}
square :: Int -> Int -> Int -> Array Int Int -> Int
square i n size a | i == 1         = a ! i'
                  | i <= n         = a ! i' - a ! modifyIndex i' n (-size) 0
                  | i `mod` n == 1 = a ! i' - a ! modifyIndex i' n 0 (-size)
                  | otherwise      = a ! i' - a ! q - a ! p + a ! r
    where i' = modifyIndex i n (size-1) (size-1)
          p  = modifyIndex i n (-1)     (size-1)
          q  = modifyIndex i n (size-1) (-1)
          r  = modifyIndex i n (-1)     (-1)

day11a :: String -> Pos
day11a s = fst $ findMaxSquare 3 300 $ grid 300 $ (read::String->Int) s  



day11b :: String -> (Int, Int, Int)
day11b s = (x,y,size')
    where (((x,y),_),size') = maximumBy (compare `on` (snd . fst)) [(findMaxSquare size 300 grd, size) | size <- [1..300]]
          grd = grid 300 $ (read::String->Int) s



-- -2  -4   4   4   4
-- -4   4   4   4  -5
--  4   3   3   4  -4
--  1   1   2   4  -3
-- -1   0   2  -5  -2






{-
day11a :: String -> Pos
day11a s = fst $ findMaxSquare 3 $ fuelGrid grid $ (read::String->Int) s

day11b :: String -> (Int, Int, Int)
day11b s = (x,y,s')
    where (s',((x,y),_)) = maximumBy (compare `on` (snd . snd)) maxSquares
          maxSquares = [(n, findMaxSquare  n $ fuelGrid grid input) | n <- [1..300]]
          input = (read::String->Int) s

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
    
sums :: Seq Cell -> Seq (Seq Int)
sums cs = do
    let sumMap = M.fromList [((1,1),cs `S.index` 0)]
        sumMap =  

asdf :: Map Pos Int -> Pos -> Map Pos Int
asdf m (x,y) = 
    
    -}
