module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map, fromList, toList)

transform :: Map Int String -> Map Char Int
transform legacyData = fromList (transform' (toList legacyData))

transform' :: [(Int, String)] -> [(Char, Int)]
transform' legacyData = 
  if legacyData == []
    then []
    else helper (head legacyData) ++ transform' (tail legacyData)

helper :: (Int, String) -> [(Char, Int)]
helper (_, []) = []
helper (val, (x:xs)) = [(toLower x, val)] ++ helper (val, xs)
