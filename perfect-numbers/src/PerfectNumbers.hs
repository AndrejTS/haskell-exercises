module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n < 1 = Nothing
  | aliquotSum == n = Just Perfect
  | aliquotSum > n  = Just Abundant
  | aliquotSum < n  = Just Deficient
  where 
    aliquotSum = sum ([x | x <- [1..(n `div` 2)], n `mod` x == 0])
