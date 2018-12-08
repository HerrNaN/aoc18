module Day08 (day08a, day08b) where

import Data.List.Split

data Node = Node {header :: (Int, Int), children :: [Node], meta :: [Int]}
    deriving (Show)

-- | Parses the input into a Node.
parseInput :: String -> Node
parseInput s = fst $ getNode $ map (read::String->Int) $ splitOn " " s

-- | Gets the outermost Node from the input.
getNode :: [Int] -> (Node, [Int])
getNode (c:m:b) | c == 0    = (Node (c,m) [] ms, b')
                | otherwise = (Node (c,m) cs ms', b'') 
    where (ms, b') = splitAt m b
          (ms', b'') = splitAt m rem
          (cs, rem) = getChildren c b []

-- | Gets a given amount of Nodes as children.
getChildren :: Int -> [Int] -> [Node] -> ([Node], [Int])
getChildren 0 bs ns = (ns, bs)
getChildren c bs ns = getChildren (c-1) bs' (ns ++ [n])
    where (n, bs') = getNode bs
 
-- | Sums the meta values of the nodes.
sumMeta :: Node -> Int
sumMeta node = sum (meta node) + sum (map sumMeta $ children node)

-- | Solves the first part of the puzzle.
day08a :: String -> Int
day08a s = sumMeta $ parseInput s

-- | Gets the value of a node.
getValue :: Node -> Int
getValue (Node (0,_) _  ms) = sum ms
getValue (Node (_,_) cs ms) = sum [getChildValue m cs | m <- ms]

-- | Gets the value of a child at a given index.
getChildValue :: Int -> [Node] -> Int
getChildValue i cs | i > length cs = 0
                   | otherwise     = getValue (cs !! (i-1))

-- | Solves the second part of the puzzle.
day08b :: String -> Int
day08b s = getValue $ parseInput s
