module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys = if length xs /= length ys
                   then Nothing
                   else Just (compare' xs ys)

compare' :: String -> String -> Int
compare' [] [] = 0
compare' (x:xs) (y:ys) = 
  if x /= y 
    then 1 + compare' xs ys
    else 0 + compare' xs ys
