module Day12 (day12a, day12b) where

import Data.Sequence (Seq (..) )
import qualified Data.Sequence as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Utils

type Plant = Char

padLeft :: [Plant] -> [Plant]
padLeft ps | endswith ".." ps = ps
           | endswith "." ps  = ps ++ "."
           | otherwise        = ps ++ ".."

padRight :: [Plant] -> [Plant]
padRight ps@('.':'.':_) = ps
padRight ps@('.':_)     = '.':ps
padRight ps             = '.':'.':ps

pad :: [Plant] -> [Plant]
pad = padRight . padLeft

step :: [Plant] -> Map [Plant] Plant -> [Plant]
step ps mps = step' (pad ps) mps

step' :: [Plant] -> [Plant] -> Map [Plant] Plant -> [Plant]
step' []               ps' mps = ps'
step' (l':l:c:r:r':ps) ps' mps = 



day12a :: String -> Int
day12a s = undefined

day12b :: String -> ()
day12b s = undefined
