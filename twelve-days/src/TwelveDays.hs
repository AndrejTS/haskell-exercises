module TwelveDays (recite) where

import Data.List (intercalate)

recite :: Int -> Int -> [String]
recite start stop = map makeVerse [start .. stop]

makeVerse :: Int -> String
makeVerse n =
  "On the "
    ++ ordinal !! (n - 1)
    ++ " day of Christmas my true love gave to me: "
    ++ gifts' n
    ++ "."
  where
    gifts' :: Int -> String
    gifts' c
      | n == 1 = head gifts
      | otherwise = stuff
      where
        stuff =
          intercalate ", " (reverse (take (c - 1) (tail gifts)))
            ++ ", and "
            ++ head gifts

ordinal :: [[Char]]
ordinal =
  [ "first",
    "second",
    "third",
    "fourth",
    "fifth",
    "sixth",
    "seventh",
    "eighth",
    "ninth",
    "tenth",
    "eleventh",
    "twelfth"
  ]

gifts :: [[Char]]
gifts =
  [ "a Partridge in a Pear Tree",
    "two Turtle Doves",
    "three French Hens",
    "four Calling Birds",
    "five Gold Rings",
    "six Geese-a-Laying",
    "seven Swans-a-Swimming",
    "eight Maids-a-Milking",
    "nine Ladies Dancing",
    "ten Lords-a-Leaping",
    "eleven Pipers Piping",
    "twelve Drummers Drumming"
  ]
