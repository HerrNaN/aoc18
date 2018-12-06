import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06


main :: IO ()
main = hspec $ do
    describe "Day 1" $ do
        it "Part A" $ day01a [1, -2, 3, 1] `shouldBe` 3
        it "Part B" $ day01b [1, -2, 3, 1] `shouldBe` 2
    describe "Day 2" $ do
        it "Part A" $ day02a [ "abcdef"
                             , "bababc"
                             , "abbcde"
                             , "abcccd"
                             , "aabcdd"
                             , "abcdee"
                             , "ababab"
                             ] `shouldBe` 12
        it "Part B" $ day02b [ "abcde"
                             , "fghij"
                             , "klmno"
                             , "pqrst"
                             , "fguij"
                             , "axcye"
                             , "wvxyz"
                             ] `shouldBe` "fgij"
    describe "Day 3" $ do
        
        it "Part A" $ do 
            example <- readFile "ex03.txt"
            day03a example `shouldBe` 4
        it "Part B" $ do
            example <- readFile "ex03.txt"
            day03b example `shouldBe` 3

    describe "Day 4" $ do 
        
        it "Part A" $ do
            example <- readFile "ex04.txt"
            day04a example `shouldBe` 240
        it "Part B" $ do
            example <- readFile "ex04.txt"
            day04b example `shouldBe` 4455
    
    describe "Day 5" $ do
        it "Part A" $ day05a "dabAcCaCBAcCcaDA" `shouldBe` 10
        it "Part B" $ day05b "dabAcCaCBAcCcaDA" `shouldBe` 4
    
    describe "Day 6" $ do
        it "Part A" $ day06a "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9" `shouldBe` 17
        it "Part B" $ day06b "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9" 32 `shouldBe` 16