module Day02
    ( day02a,
      day02b
    ) where
import Data.Map (fromListWith, toList)
import Data.Maybe

-- Counts the occurances of each element.
frequency :: (Ord a) => [a] -> [(a,Int)]
frequency xs = toList (fromListWith (+) [(x,1) | x <- xs])

-- Evaluates whether the string has any characters
-- appearing exactly twice or exactly thrice.
hasDupsOrTrips :: String -> (Bool,Bool)
hasDupsOrTrips s = (dups,trips)
    where dups   = any ((==2) . snd) freqs
          trips  = any ((==3) . snd) freqs
          freqs  = frequency s

-- Reads the input into a managable format.
readInput :: FilePath -> IO [String]
readInput f = lines <$> readFile f

-- Solves the first part of the puzzle.
day02a :: [String] -> Int
day02a ss = sumBools dups * sumBools trips
    where sumBools = foldr ((+) . fromEnum) 0
          (dups,trips) = unzip $ map hasDupsOrTrips ss

-- Evaluates whether the two strings differ by
-- exactly one character.
diffBy1 :: String -> String -> Bool
diffBy1 s s' = diffBy1' s s' 0
    where diffBy1' []     []     0 = False
          diffBy1' []     []     _ = True
          diffBy1' (x:xs) (y:ys) 0 = diffBy1' xs ys (fromEnum (x /= y))
          diffBy1' (x:xs) (y:ys) 1 | x /= y = False
                                   | otherwise = diffBy1' xs ys 1

-- Finds the pair of strings in the list that
-- differ by exactly one character, given a list
-- of string where such a pair exists.
diffPair :: [String] -> (String,String)
diffPair (x:xs) = fromMaybe (diffPair xs) (pair x xs)

-- Returns the given string and, where one exists,
-- the string which differs from the given by exactly
-- one character.
pair :: String -> [String] -> Maybe (String,String)
pair _ []     = Nothing
pair s (x:xs) | diffBy1 s x = Just (s,x)
              | otherwise   = pair s xs

-- Returns a string containing only the characters
-- that where identical between two strings equal
-- in length.
commonChars :: String -> String -> String
commonChars []     []     = []
commonChars (x:xs) (y:ys) | x == y    = x : commonChars xs ys
                          | otherwise = commonChars xs ys

-- Solves the second part of the puzzle.
day02b :: [String] -> String
day02b ss = uncurry commonChars $ diffPair ss
