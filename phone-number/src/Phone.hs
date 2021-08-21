module Phone (number) where

import Data.Char (digitToInt)

number :: String -> Maybe String
number xs
  | length filtered == 10
      && checkCodes filtered =
    Just filtered
  | length filtered == 11
      && head filtered == '1'
      && checkCodes (drop 1 filtered) =
    Just (drop 1 filtered)
  | otherwise = Nothing
  where
    filtered = filter (`elem` ['0' .. '9']) xs

checkCodes :: String -> Bool
checkCodes n =
  isTwoThroughNine (head n) && isTwoThroughNine (n !! 3)

isTwoThroughNine :: Char -> Bool
isTwoThroughNine ch
  | digitToInt ch `elem` [2 .. 9] = True
  | otherwise = False
