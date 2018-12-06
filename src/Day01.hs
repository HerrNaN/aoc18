module Day01 (day01a,day01b) where

import Data.Map (Map, insertLookupWithKey, empty)

-- Parses the input.
parseInput :: String -> [Int]
parseInput = map (read . dropWhile (=='+')) . lines

-- Solves the first part of the puzzle given a
-- valid input at 'f'.
day01a :: String -> Int
day01a s = sum . map (read . dropWhile (=='+')) $ lines s

-- Counts the number of occurances of values for each frequency change
-- and return the first value that occur twice.
firstRepeat :: Int -> Map Int Int -> [Int] -> [Int] -> Int
firstRepeat p m o []     = firstRepeat p m o o
firstRepeat p m o (f:fs) = if v == Just 1 then p else firstRepeat (p+f) m' o fs
    where (v,m')          = insertLookupWithKey inc p 1 m
          inc key new old = new + old

-- Solves the second part of the puzzle given a
-- valid input at 'f'.
day01b :: String -> Int
day01b s = firstRepeat 0 empty is is
    where is = parseInput s
