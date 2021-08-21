module Triangle (TriangleType (..), triangleType) where

import Data.List (nub)

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illegal
  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | not (isTriangle a b c) = Illegal
  | uniqueSides == 1 = Equilateral
  | uniqueSides == 2 = Isosceles
  | uniqueSides == 3 = Scalene
  where
    uniqueSides = length (nub [a, b, c])

isTriangle :: (Num a, Ord a) => a -> a -> a -> Bool
isTriangle a b c
  | 0 `elem` [a, b, c] = False
  | a + b < c = False
  | a + c < b = False
  | b + c < a = False
  | otherwise = True
