import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Day01 (day01a, day01b)
import Day02 (day02a, day02b)


main :: IO ()
main = hspec $ do
    describe "Day 1" $ do
        it "Part A" $ day01a [1, -2, 3, 1] `shouldBe` 3
        it "Part B" $ day01b [1, -2, 3, 1] `shouldBe` 2
    describe "Day 2" $ do
        it "Part A" $ do
            day02a [ "abcdef"
                   , "bababc"
                   , "abbcde"
                   , "abcccd"
                   , "aabcdd"
                   , "abcdee"
                   , "ababab"
                   ] `shouldBe` 12
        it "Part B" $ do
            day02b [ "abcde"
                   , "fghij"
                   , "klmno"
                   , "pqrst"
                   , "fguij"
                   , "axcye"
                   , "wvxyz"
                   ] `shouldBe` "fgij"