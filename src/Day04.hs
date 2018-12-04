module Day04 (day04a, day04b) where

import Data.List hiding (insert)
import Data.Map.Strict hiding (map, drop, take)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Function (on)

data EntryType = B | S | W
    deriving (Show, Eq)
data Entry = Entry Int EntryType 
    deriving (Show, Eq)


parseInput :: String -> [String]
parseInput s = sort $ lines s

readEntry :: String -> Entry
readEntry s | "falls asleep" `isInfixOf` s = Entry (read(minutes s) :: Int) S
            | "wakes up" `isInfixOf` s     = Entry (read(minutes s) :: Int) W
            | otherwise                    = Entry (read(gid s)     :: Int) B
    where minutes = take 2 . drop 1 . dropWhile (/=':')
          gid     = takeWhile (/=' ') . drop 1 . dropWhile (/='#')

readAllEntries :: [String] -> [Entry]
readAllEntries = map readEntry

sleepyG :: [Entry] -> Int -> Map Int (Map Int Int) -> Map Int (Map Int Int)
sleepyG []                        _ m = m
sleepyG (Entry n B:es)            c m = sleepyG es n m
sleepyG (Entry n S:Entry n' W:es) c m = sleepyG es c m'
    where m'  = insert c (incValuesWithKeys [n..(n'-1)] v') m
          v   = m !? c
          v'  = case v of
                     Just x -> x
                     _      -> empty

incValuesWithKeys :: [Int] -> Map Int Int -> Map Int Int
incValuesWithKeys []     m = m
incValuesWithKeys (k:ks) m = incValuesWithKeys ks $ insertWith inc k 1 m
    where inc n o = o + n

values :: Map Int (Map Int Int) -> (Int, Int)
values m = (guard, minute)
    where (guard, _)  = maximumBy (compare `on` snd) [(g, sum $ elems $ m ! g) | g <- keys m]
          (minute, _) = maximumBy (compare `on` snd) $ zipMap $ m ! guard
          zipMap a    = zip (keys a) (elems a)

idMin :: Map Int (Map Int Int) -> (Int, Int)
idMin m = (id, min)
    where m' = M.map (maximumBy (compare `on` snd) . M.toList) m
          (id, (min, _)) = maximumBy (compare `on` (snd . snd)) $ M.toList m'

day04a :: String -> Int
day04a s = uncurry (*) $ values $ sleepyG (readAllEntries $ parseInput s) 0 empty

day04b :: String -> Int
day04b s = uncurry (*) $ idMin $ sleepyG (readAllEntries $ parseInput s) 0 empty