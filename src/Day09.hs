module Day09 (day09a, day09b) where

import Data.Char
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as S

type Player = Int
type Turn   = Int
type Score  = Int
type Marble = Int

parseInput :: String -> (Int, Int)
parseInput s = ((read::String->Int) p, (read::String->Int) l)
    where s'       = filter (\c -> isDigit c || isPunctuation c) s
          (p:l:cs) = splitOn ";" s'

{-
    Current turn    ->
    Last turn       ->
    Current marble  ->
    # of Players    ->
    Marbles in play ->
    Score table     ->
    Score table
-}
playMarble :: Turn -> Turn -> Marble -> Int -> Seq Marble -> Map Player Score -> Map Player Score
playMarble t max cur ps ms sc | max == t        = sc
                              | t `mod` 23 == 0 = playMarble (t+1) max cur'  ps ms'  sc'
                              | otherwise       = playMarble (t+1) max cur'' ps ms'' sc
    where (cur'', ms'')    = play t cur ms
          (cur', ms', sc') = score t cur ps ms sc

score :: Turn -> Marble -> Int -> Seq Marble -> Map Player Score -> (Marble, Seq Marble, Map Player Score)
score t cur ps ms sc = (cur', ms', sc')
    where cur' = if (cur - 7) == -1
                    then (cur - 7) `mod` (length ms + 1)
                    else (cur - 7) `mod` length ms
          ms'  = S.deleteAt cur' ms
          sc'  = M.insertWith (+) (t `mod` ps) (t + S.index ms cur') sc

play :: Turn -> Marble -> Seq Marble -> (Marble, Seq Marble)
play t cur ms = (cur', ms')
    where cur' = if (cur + 2) == length ms 
                    then cur + 2 
                    else (cur + 2) `mod` length ms
          ms'  = S.insertAt cur' t ms

day09a :: String -> Int
day09a s = maximum $ M.elems scoreTable
    where (players, max) = parseInput s
          scoreTable     = playMarble 1 max 0 players (S.singleton 0)  M.empty

day09b :: String -> Int
day09b s = maximum $ M.elems scoreTable
    where (players, max) = parseInput s
          scoreTable     = playMarble 1 (max * 100) 0 players (S.singleton 0)  M.empty 