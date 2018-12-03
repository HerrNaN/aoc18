module Day01 (day01a,day01b) where

import Data.Map (Map, insertLookupWithKey, empty)

-- Reads a file with formatted input and returns
-- a list with frequency changes.
readFreqs :: FilePath -> IO [Int]
readFreqs f = map (read . dropWhile (=='+')) . lines <$> readFile f

-- Solves the first part of the puzzle given a
-- valid input at 'f'.
day01a :: [Int] -> Int
day01a = sum 

-- Counts the number of occurances of values for each frequency change
-- and return the first value that occur twice.
firstRepeat :: Int -> Map Int Int -> [Int] -> [Int] -> Int
firstRepeat p m o []     = firstRepeat p m o o
firstRepeat p m o (f:fs) = if v == Just 1 then p else firstRepeat (p+f) m' o fs
    where (v,m')          = insertLookupWithKey inc p 1 m
          inc key new old = new + old

-- Solves the second part of the puzzle given a
-- valid input at 'f'.
day01b :: [Int] -> Int
day01b is = firstRepeat 0 empty is is
