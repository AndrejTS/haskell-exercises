module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = 
  sum (filter (isMultiple factors) [1..limit-1])

isMultiple :: [Integer] -> Integer -> Bool
isMultiple [] _ = False
isMultiple (f:fs) n
  | f == 0 || n `mod` f /= 0 = isMultiple fs n
  | otherwise = True
