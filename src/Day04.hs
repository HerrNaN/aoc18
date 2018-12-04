module Day04 (day04a, day04b) where

import Data.List
import Data.Map.Strict (Map, adjust, keys, elems, empty, (!))
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

sleepyG :: [Entry] -> Int -> Map Int (Map Int Int) -> (Int,Int)
sleepyG []                        _ m = values m
sleepyG (Entry n B:es)            c m = sleepyG es n m
sleepyG (Entry n S:Entry n' W:es) c m = sleepyG es c m'
    where m'  = adjust (incValuesWithKeys [n..n']) c m

incValuesWithKeys :: [Int] -> Map Int Int -> Map Int Int
incValuesWithKeys [] m = m
incValuesWithKeys ks m = foldl (flip (adjust (1 +))) m ks

values :: Map Int (Map Int Int) -> (Int, Int)
values m = (guard, minute)
    where (guard, _)  = maximumBy (compare `on` snd) [(g,sum $ elems ms) | (g,ms) <- zip (keys m) (elems m)]
          (minute, _) = maximumBy (compare `on` snd) $ zip (keys ts) (elems ts)
          ts          = m ! guard
          

day04a :: String -> Int
day04a s = uncurry (*) $ sleepyG (readAllEntries $ parseInput s) 0 empty

day04b = undefined