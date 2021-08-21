module ResistorColors (Color (..), Resistor (..), label, ohms) where

data Color
  = Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor {bands :: (Color, Color, Color)}
  deriving (Show)

label :: Resistor -> String
label Resistor {bands = (a, b, c)} = shrinked ++ " " ++ unit
  where
    ohms' = ohms (Resistor {bands = (a, b, c)})
    shrinked = show (shrink ohms')
    unit = getUnit ohms'

ohms :: Resistor -> Int
ohms Resistor {bands = (a, b, c)} =
  (fromEnum a * 10 + fromEnum b) * (10 ^ fromEnum c)

shrink :: Int -> Int
shrink n =
  if n `div` 1000 > 0
    then shrink (n `div` 1000)
    else n

getUnit :: Int -> String
getUnit n
  | n >= (1 * 10 ^ 12) = error "Too much ohms!"
  | n `div` (1 * 10 ^ 9) > 0 = "gigaohms"
  | n `div` (1 * 10 ^ 6) > 0 = "megaohms"
  | n `div` (1 * 10 ^ 3) > 0 = "kiloohms"
  | otherwise = "ohms"
