import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Day01
import Day02
import Day03


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
        it "Part A" $ day03a "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2" `shouldBe` 4
        it "Part B" $ day03b "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2" `shouldBe` 3 