module Main where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07

getInput :: Int -> IO String
getInput n | n < 10    = readFile $ "inputs/day0" ++ show n ++ ".txt"
           | otherwise = readFile $ "inputs/day" ++ show n ++ ".txt"

solutions :: [(Int, String -> String, String -> String)]
solutions = [(1, show . day01a, show . day01b),
             (2, show . day02a, show . day02b),
             (3, show . day03a, show . day03b),
             (4, show . day04a, show . day04b),
             (5, show . day05a, show . day05b),
             (6, show . day06a, show . day06b)
             ]

printSolution :: (Int, String -> String, String -> String) -> IO ()
printSolution (day, partA, partB) = do
                    input <- getInput day
                    putStrLn $ "Day " ++ show day
                    putStrLn $ "  Part A: " ++ partA input
                    putStrLn $ "  Part B: " ++ partB input


main :: IO ()
main = mapM_ printSolution solutions

    
