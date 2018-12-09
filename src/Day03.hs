module Day03
    (day03a,
     day03b
    ) where

import Data.List 
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M 
import Data.Maybe

type Point = (Int, Int)
data Claim = Claim {cid :: Int, pos :: Point, width :: Int, height :: Int}
        deriving (Eq, Show)

-- Counts the occurances of each element.
frequency :: (Ord a) => [a] -> [(a,Int)]
frequency xs = M.toList (M.fromListWith (+) [(x,1) | x <- xs])

-- Collects all Claims from a string.
getAllClaims :: String -> [Claim]
getAllClaims s = map getClaim (lines s)

-- Converts a line to a Claim
getClaim :: String -> Claim
getClaim s = Claim cid (x,y) w h
    where pts = split (dropBlanks $ dropDelims $ oneOf " :#@,x") s
          cid  = getIntN 0 pts
          x   = getIntN 1 pts
          y   = getIntN 2 pts
          w   = getIntN 3 pts
          h   = getIntN 4 pts

-- Gets the nth element in a list
getIntN :: Int -> [String] -> Int
getIntN n s = read (head $ take 1 $ drop n s)

-- Gets the points allocated by a given Claim
getPoints :: Claim -> [Point]
getPoints c = [(x+dx, y+dy) | dx <- [0..(w-1)], dy <- [0..(h-1)]]
    where (x,y) = pos c
          w     = width c
          h     = height c

-- Solves the first part of the puzzle
day03a :: String -> Int
day03a s = length . filter ((>1) . snd) $ frequency points
        where claims = getAllClaims s
              points = concatMap getPoints claims

findNoOverlap :: [Claim] -> Claim
findNoOverlap cs = findNoOverlap' cs m
    where pts = [(p,1) | p <- concat [getPoints c | c <- cs]]
          m   = M.fromListWith (+) pts

findNoOverlap' :: [Claim] -> Map Point Int -> Claim
findNoOverlap' (c:cs) m | all (== 1) [m M.! p | p <- getPoints c] = c
                        | otherwise = findNoOverlap' cs m

-- Solves the second part of the puzzle
day03b :: String -> Int
day03b s = cid $ findNoOverlap $ getAllClaims s