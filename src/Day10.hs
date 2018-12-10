module Day10 (day10a, day10b) where

import Data.List
import Data.List.Utils (replace)
import Data.List.Split
import Data.Ord (comparing)
import Data.Function (on)


type Pos = (Int, Int)
type Vel = (Int, Int)
data Light = Light{ position :: Pos, velocity :: Vel }

update :: Light -> Light
update (Light (x,y) (v,u)) = Light (x+v, y+u) (v,u)

showLights :: [Light] -> IO ()
showLights ls = putStrLn $ lightsGrid ls

lightsGrid :: [Light] -> String
lightsGrid ls = unlines [ [ if (x,y) `elem` pss then '#' else '.' | x <- [minX..maxX] ]
                                                                  | y <- [minY..maxY] ]
    where pss = map position ls
          maxX = fst $ maximumBy (compare `on` fst) pss
          maxY = snd $ maximumBy (compare `on` snd) pss
          minX = fst $ minimumBy (compare `on` fst) pss
          minY = snd $ minimumBy (compare `on` snd) pss

parseInput :: String -> [Light]
parseInput s = map parseLine $ lines s

parseLine :: String -> Light
parseLine s = Light (x,y) (v,u)
    where [(x,y),(v,u)] = map ((read::String->(Int,Int)) . dropWhile (/='(')) 
                          $ (splitOn "v" . replace "<" "(" . replace ">" ")") s

lightDiff :: [Light] -> Int
lightDiff ls = abs (maxY - minY) 
    where ps   = sortBy (compare `on` snd) $ map position ls
          maxY = snd $ last ps
          minY = snd $ head ps

wait :: Int -> [Light] -> Int -> (Int, [Light])
wait diff ls t   | diff <= nextDiff = (t, ls)
                 | otherwise        = wait nextDiff ls' $ t + 1
    where ls'  = map update ls
          diff = lightDiff ls
          nextDiff = lightDiff ls'
                
day10a :: String -> String
day10a s = lightsGrid $ snd $ wait 1000000 (parseInput s) 0

day10b :: String -> Int
day10b s = fst $ wait 1000000 (parseInput s) 0
