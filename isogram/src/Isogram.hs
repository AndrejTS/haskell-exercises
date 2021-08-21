module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)

isIsogram :: String -> Bool
isIsogram str = sum (map (count str') str') == length str'
  where str' = map toLower (filter isLetter str)

count :: String -> Char -> Int
count str c = length (filter (== c) str)
