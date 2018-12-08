module Day07 (day07a, day07b, day07bTest) where

import Data.List (map, (\\), filter, null, sort, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import Debug.Trace

{-# ANN module "HLint: ignore Use String" #-}

type Vert = (Char, Char)

parseInput :: String -> [Vert]
parseInput s = map parseLine $ lines s
    where parseLine s' = (s' !! 5, s' !! 36)

findStarts :: [Vert] -> [Char]
findStarts cs = sort $ nub (as ++ bs) \\ nub bs
    where (as, bs) = unzip cs

findEnds :: [Vert] -> [Char]
findEnds cs = sort $ nub (as ++ bs) \\ nub as
    where (as, bs) = unzip cs

trav :: [Vert] -> [Char] -> [Char]
trav reqs ts | null reqs' = ts ++ sort as ++ sort bs
             | otherwise  = trav reqs' ts'
    where (s:_) = findStarts reqs
          ts' = ts ++ [s]
          reqs' = filter ((/=s) . fst) reqs
          (as, bs) = unzip reqs

day07a :: String -> String
day07a s = trav (parseInput s) ""

type Work = (Char, Int)

decWork :: Int -> Work -> Work
decWork n (c,t) = (c,t-n)

decWorkers :: Int -> [Work] -> [Work]
decWorkers n = map (decWork n)

getTime :: Int -> Char -> Int
getTime n c = ord c - 64 + n

completed :: [Work] -> [Char]
completed ws = map fst $ filter ((==0) . snd) ws

parTrav :: [Vert] -> [Work] -> Int -> Int -> Int-> Int
parTrav vs ws offset workerMax t | null ws' = t
                                 | otherwise = parTrav vs' (decWorkers timeStep ws') offset workerMax $ t + timeStep
    where free        = findStarts vs \\ map fst working
          cs          = completed (decWorkers timeStep ws')
          vs'         = [(v, v') | (v,v') <- vs, v `notElem` cs]
          nws         = [(w, getTime offset w) | w <- take idleWorkers free]
          ws'         = working ++ nws
          timeStep    = minimum $ map snd ws'
          idleWorkers = workerMax - length working
          working     = filter ((/=0) . snd) ws

solve07b :: String -> Int -> Int -> Int
solve07b s offs workerMax = parTrav vs' [] offs workerMax 0
    where vs = parseInput s
          vs' = vs ++ [(f, 'e') | f <- fs]
          fs = findEnds vs

day07b :: String -> Int
day07b s = solve07b s 60 5

day07bTest :: String -> Int
day07bTest s = solve07b s 0 2
