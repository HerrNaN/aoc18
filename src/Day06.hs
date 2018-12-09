module Day06 (day06a, day06b, day06bTest) where

import Data.List
import Data.List.Split
import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe

type Pos = (Int, Int)

parseInput :: String -> [Pos]
parseInput s = map getPos $ lines s

getPos :: String -> Pos
getPos s = (x, y)
    where x = read (takeWhile (/=',') s) :: Int
          y = read (dropWhile (/=' ') s) :: Int

manhattan :: Pos -> Pos -> Int
manhattan (x,y) (x',y') = abs (x'-x) + abs (y'-y)

boundary :: [Pos] -> (Pos, Int, Int)
boundary ps = ((minX - 1, minY-1), maxX - minX + 2, maxY - minY +2)
    where minX = fst $ minimumBy (compare `on` fst) ps
          maxX = fst $ maximumBy (compare `on` fst) ps
          minY = snd $ minimumBy (compare `on` snd) ps
          maxY = snd $ maximumBy (compare `on` snd) ps
          
nearest :: Pos -> [Pos] -> Maybe Pos
nearest p ps = if d == d' then Nothing else Just p1
    where ((p1, d):(_, d'):ns) = sortOn snd [(p', manhattan p p') | p' <- ps]

markedGrid :: [Pos] -> [Maybe Pos]
markedGrid ps = map (`nearest` ps) $ grid ps

grid :: [Pos] -> [Pos]
grid ps = [(x', y') | x' <- [x..(x+w)], y' <- [y..(y+h)]]
    where ((x,y), w, h) = boundary ps

finiteAreas :: [Maybe Pos] -> [Pos] -> [Maybe Pos]
finiteAreas areas ps = areas \\ infAreas
    where frame = [(x',y') | x' <- [x..(x+w)], y' <- [y..(y+h)], 
                                    x' == x+w || x' == x ||
                                    y' == y+h || y' == y]
          ((x,y), w, h) = boundary ps
          infAreas = map (`nearest` ps) frame

day06a :: String -> Int
day06a s = maximum $ map length $ group $ sort $ catMaybes $ finiteAreas (markedGrid ps) ps
    where ps = parseInput s

sumManhattans :: [Pos] -> Pos -> Int
sumManhattans pts p = sum $ map (manhattan p) pts 

day06b :: String -> Int
day06b s = length $ filter ((<10000) . sumManhattans ps) $ grid ps
    where ps = parseInput s

day06bTest :: String -> Int
day06bTest s = length $ filter ((<32) . sumManhattans ps) $ grid ps
    where ps = parseInput s