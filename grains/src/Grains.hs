module Grains (square, total) where

import Data.Maybe

square :: Integer -> Maybe Integer
square n = 
  if 0 >= n || n > 64
    then Nothing
    else Just (2 ^ (n - 1))

total :: Integer
total = 18446744073709551615
-- total = sum [fromJust (square x) | x <- [1..64]]
