module Main where

import Lib
import Day06

main :: IO ()
main = do
    input <- readFile "day06.txt"
    print (day06b input 10000)
