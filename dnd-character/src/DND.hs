module DND
  ( Character (..),
    ability,
    modifier,
    character,
  )
where

import Test.QuickCheck (Gen, choose)

data Character = Character
  { strength :: Int,
    dexterity :: Int,
    constitution :: Int,
    intelligence :: Int,
    wisdom :: Int,
    charisma :: Int,
    hitpoints :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier =
  error "You need to implement this function."

ability :: Gen Int
ability =
  choose (1, 10)

character :: Gen Character
character =
  error "You need to implement this generator."
