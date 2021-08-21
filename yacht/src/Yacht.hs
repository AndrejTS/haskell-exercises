module Yacht (yacht, Category (..)) where

import Data.List (group, nub, sort)

data Category
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht

yacht :: Category -> [Int] -> Int
yacht category dice = checkIt
  where
    checkIt = case category of
      Ones -> countBasic dice 1
      Twos -> countBasic dice 2
      Threes -> countBasic dice 3
      Fours -> countBasic dice 4
      Fives -> countBasic dice 5
      Sixes -> countBasic dice 6
      FullHouse -> fullHouse dice
      FourOfAKind -> fourOfAKind dice
      LittleStraight ->
        if testStraight dice
          && minimum dice == 1
          then 30
          else 0
      BigStraight ->
        if testStraight dice
          && minimum dice == 2
          then 30
          else 0
      Choice -> sum dice
      Yacht ->
        if length (nub dice) == 1
          then 50
          else 0

-- for Ones, Twos, ... Sixes
countBasic :: [Int] -> Int -> Int
countBasic dice n = length (filter (== n) dice) * n

fullHouse :: [Int] -> Int
fullHouse dice =
  if (length uniques == 2)
    -- one value must be twice and one three times
    && length (filter (== head uniques) dice) `elem` [2, 3]
    then sum dice
    else 0
  where
    uniques = nub dice

fourOfAKind :: [Int] -> Int
fourOfAKind dice =
  if length uniques `elem` [1, 2]
    && length (filter (== head uniques) dice) `elem` [4, 5]
    then 4 * mostFrequent dice
    else 0
  where
    uniques = nub dice

mostFrequent :: [Int] -> Int
mostFrequent ns =
  snd (maximum [(length ks, head ks) | ks <- group (sort ns)])

testStraight :: [Int] -> Bool
testStraight dice =
  length uniques == 5
    && head sorted == last sorted - 4
  where
    uniques = nub dice
    sorted = sort dice
