module Main where

import Days

main :: IO ()
main = mapM_ printSolution solutions

{-|
  The solutions, ready to run with the input. The format 
  is as follows:

  (Day, solution A, solution B)
-}
solutions :: [(Int, String -> String, String -> String)]
solutions = [
             (1, show . day01a, show . day01b)
            ,(2, show . day02a, show . day02b)
            ,(3, show . day03a, show . day03b)
            ,(4, show . day04a, show . day04b)
            ,(5, show . day05a, show . day05b)
            ,(6, show . day06a, show . day06b)
            ,(7, show . day07a, show . day07b)
            ,(8, show . day08a, show . day08b)
            ,(9, show . day09a, show . day09b)
             ]

-- | Formats the solution outputs for a given day in a nice way.
printSolution :: (Int, String -> String, String -> String) -> IO ()
printSolution (day, partA, partB) = do
                    input <- getInput day
                    putStrLn $ "Day " ++ show day
                    putStrLn $ "  Part A: " ++ partA input
                    putStrLn $ "  Part B: " ++ partB input

-- | Reads the input for a given day.
getInput :: Int -> IO String
getInput n | n < 10    = readFile $ "inputs/day0" ++ show n ++ ".txt"
           | otherwise = readFile $ "inputs/day" ++ show n ++ ".txt"