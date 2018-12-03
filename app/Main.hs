module Main where

import Lib
import Day03

main :: IO ()
main = do
    s2 <- show <$> (day03b <$> readFile "day03.txt")
    putStrLn s2
