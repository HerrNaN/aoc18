module Day11 where

import Data.List
import Data.Ord (comparing)
import Data.Function (on)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as M
import Control.DeepSeq



type Serial = Int
type Pos  = (Int, Int)
{- data Cell = Cell {position :: Pos, power :: Int}
    deriving Show -}

getPower :: Pos -> Serial -> Int
getPower (x,y) serial = hd - 5
    where rid = x + 10
          pws = (rid * y + serial) * rid
          hd  = (pws `div` 100) `mod` 10

grid :: Int -> Serial -> Matrix Int
grid n s = M.fromList n n [getPower (x,y) s | x <- [1..n], y <- [1..n]]

-- Eats all your memory when 
preProcessed :: Matrix Int -> Matrix Int
preProcessed mx = foldl' ppCell mx' [ (r,c) | c <- reverse [2..(M.nrows mx)],
                                              r <- reverse [2..(M.ncols mx)] ]
    where mx'        = (ppRow 2 (M.ncols mx) . ppCol 2 (M.nrows mx)) mx

ppRow ::  Int -> Int -> Matrix Int -> Matrix Int
ppRow n m mx | n == m    = mx'
             | otherwise = ppRow (n + 1) m mx'
    where mx' = M.setElem ((mx ! (1,n-1)) + mx ! (1,n)) (1,n) mx

ppCol :: Int -> Int -> Matrix Int -> Matrix Int
ppCol n m mx | n == m    = mx'
             | otherwise = ppCol (n + 1) m mx'
    where mx' = M.setElem ((mx ! (n-1,1)) + mx ! (n,1)) (n,1) mx

ppCell :: Matrix Int -> Pos -> Matrix Int
ppCell mx (r,c) | c `mod` 10 == 0 = M.forceMatrix mx'
                | otherwise               = mx'
    where mx' = M.setElem (mx ! (r,     c - 1) 
                         + mx ! (r - 1, c    ) 
                         + mx ! (r,     c    ) 
                         - mx ! (r - 1, c - 1)) (r,c) mx

getSum :: Pos -> Int -> Matrix Int -> Int
getSum (r,c) size mx | r == 1 && c == 1 = mx ! (r',c')
                     | r == 1    = mx ! (r',c') - mx ! (r' , c-1)
                     | c == 1    = mx ! (r',c') - mx ! (r-1, c' )
                     | otherwise = mx ! (r'  , c' ) 
                                 - mx ! (r'  , c-1)
                                 - mx ! (r   , c' )
                                 + mx ! (r-1 , c-1)
    where r' = r + size - 1
          c' = c + size - 1

findMaxSquare :: Int -> Matrix Int -> (Pos, Int)
findMaxSquare size mx = maximumBy (compare `on` snd) [ ((r,c), getSum (r,c) size mx) | c <- [1..(300-size)], 
                                                                                       r <- [1..(300-size)] ]



deepFoldl' f z [] = z
deepFoldl' f z (x:xs) = let z' = z `f` x
                        in deepseq z' $ deepFoldl' f z' xs

day11a :: String -> Pos
day11a s = fst $ findMaxSquare 3 $ preProcessed $ grid 300 $ (read::String->Int) s  



day11b :: String -> (Int, Int, Int)
day11b s = undefined










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
