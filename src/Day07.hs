module Day07 (day07a, day07b) where

import Data.List
import Data.Map hiding (map, (\\), filter)
import qualified Data.Map as Map

parseInput :: String -> [(String, String)]
parseInput s = map parseLine $ lines s
    where parseLine s' = ([s' !! 5], [s' !! 36])

findStarts :: [(String,String)] -> String
findStarts cs = sort $ nub (concat as ++ concat bs) \\ nub (concat bs)
    where (as, bs) = unzip cs

trav :: [(String, String)] -> String -> String
trav reqs ts | 
     trav (filter ((/=[s]) . fst) reqs) (ts ++ [s])
    where (s:ss) = findStarts reqs


day07a :: String -> String
day07a s = trav (parseInput s) ""

day07b :: String -> ()
day07b s = undefined
