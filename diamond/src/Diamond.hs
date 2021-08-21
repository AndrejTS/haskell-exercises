module Diamond (diamond) where

import Data.Char (ord, isAlpha)

diamond :: Char -> Maybe [String]
diamond inputLetter
  | not (isAlpha inputLetter) = Nothing
  | otherwise = 
    if rows == 1
      then Just ["A"]
      else Just (half ++ tail (reverse half))
    where rows = ord inputLetter - 64
          width = rows * 2 - 1
          half = map (makeRow width) (take rows ['A'..])

makeRow :: Int -> Char -> String
makeRow width letter
  | letter == 'A' = 
      let marginSpaces = take (width `div` 2) spaces
      in marginSpaces ++ "A" ++ marginSpaces
  | otherwise = 
      let marginSpaces = take ((width `div` 2) - (ord letter - 65)) spaces
          innerSpaces = take ([1,3..] !! (ord letter - 66)) spaces
      in marginSpaces ++ [letter] ++ innerSpaces ++ [letter] ++ marginSpaces
  where spaces = [' ', ' '..]

