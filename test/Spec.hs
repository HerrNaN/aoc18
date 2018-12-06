import Test.Hspec

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07

main :: IO ()
main = hspec $ describe "Advent of Code 2018" $ mapM_ testDay tests

tests :: [(Int, (String -> String, String), (String -> String, String))]
tests = [(1,  (show . day01a, "3"),   (show . day01b, "2"))
        ,(2,  (show . day02a, "12"),  (day02b, "abcde"))
        ,(3,  (show . day03a, "4"),   (show . day03b, "3"))
        ,(4,  (show . day04a, "240"), (show . day04b, "4455"))
        ,(5,  (show . day05a, "10"),  (show . day05b, "4"))
        ,(6,  (show . day06a, "17"),  (show . day06bTest, "16"))
        ,(7,  (show . day07a, ""),    (show . day07b, ""))
        ]

testDay :: (Int, (String -> String, String), (String -> String, String)) -> Spec
testDay (n, (partA, ansA), (partB, ansB)) = 
    context ("Day " ++ show n) $ do
        it "Part A" $ do 
            ex <- readEx n
            partA ex `shouldBe` ansA
        it "Part B" $ do 
            ex <- readEx n
            partB ex `shouldBe` ansB

readEx :: Int -> IO String
readEx day = readFile $ "examples/day" ++ day' ++ ".txt"
    where day' = if day < 10 then "0" ++ show day else show day