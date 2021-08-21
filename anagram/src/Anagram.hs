module Anagram (anagramsFor) where

import Data.Char (toUpper)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor word =
  filter (check word)

check :: String -> String -> Bool
check x y =
  sort x' == sort y'
    && x' /= y'
  where
    x' = map toUpper x
    y' = map toUpper y
