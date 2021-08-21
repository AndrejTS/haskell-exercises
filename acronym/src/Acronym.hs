module Acronym (abbreviate) where

import Data.Char (isLetter, isLower, isUpper, toUpper)

abbreviate :: String -> String
abbreviate xs =
  map
    getFirst
    ((words . removeNonLetters . replaceDashes . camelcase) xs)

-- "HyperText Markup Language" --> "Hyper Text Markup Language"
camelcase :: String -> String
camelcase "" = ""
camelcase (x : xs) =
  if not (null xs)
    then
      if isLower x && isUpper (head xs)
        then x : ' ' : camelcase xs
        else x : camelcase xs
    else [x]

replaceDashes :: String -> String
replaceDashes = map (\x -> if x == '-' then ' ' else x)

removeNonLetters :: String -> String
removeNonLetters = filter (\x -> isLetter x || x == ' ')

getFirst :: String -> Char
getFirst [] = ' '
getFirst (x : _) = toUpper x
