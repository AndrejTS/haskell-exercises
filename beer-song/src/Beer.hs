module Beer (song) where

import Data.List (intercalate)

chunkA :: [Char]
chunkA = " bottles of beer on the wall, "

chunkB :: [Char]
chunkB = " bottles of beer."

chunkC :: [Char]
chunkC = "Take one down and pass it around, "

chunkD :: [Char]
chunkD = " bottles of beer on the wall."

song' :: Int -> String
song' 2 =
  "2 bottles of beer on the wall, 2 bottles of beer.\n\
  \Take one down and pass it around, 1 bottle of beer on the wall.\n"
song' 1 =
  "1 bottle of beer on the wall, 1 bottle of beer.\n\
  \Take it down and pass it around, no more bottles of beer on the wall.\n"
song' 0 =
  "No more bottles of beer on the wall, no more bottles of beer.\n\
  \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
song' n =
  firstRow ++ secondRow
  where
    initCount = show n
    nextCount = show (n - 1)
    firstRow = initCount ++ chunkA ++ initCount ++ chunkB ++ "\n"
    secondRow = chunkC ++ nextCount ++ chunkD ++ "\n"

song :: String
song = intercalate "\n" $ map song' [99, 98 .. 0]
