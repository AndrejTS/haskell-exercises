module Bob (responseFor) where

import Data.Char (isLetter, isUpper)
import Data.List (intercalate)

isAllUppercase :: [Char] -> Bool
isAllUppercase xs =
  any isLetter xs && all isUpper (filter isLetter xs)

responseFor :: String -> String
responseFor xs
  | null strWithoutSpaces = "Fine. Be that way!"
  | strWithoutSpaces !! (length strWithoutSpaces - 1) == '?' =
    if isAllUppercase xs
      then "Calm down, I know what I'm doing!"
      else "Sure."
  | any isLetter xs && isAllUppercase xs = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    strWithoutSpaces = intercalate "" $ words xs
