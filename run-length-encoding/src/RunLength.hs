module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group, intercalate)

decode :: String -> String
decode [] = ""
decode encodedText =
  replicate count (head cleaned) ++ decode (tail cleaned)
  where
    digits = takeWhile isDigit encodedText
    count =
      if not (null digits)
        then read digits :: Int
        else 1
    cleaned = dropWhile isDigit encodedText

encode :: String -> String
encode text = intercalate "" (map helper group')
  where
    group' = group text
    helper x =
      let multiplier = show (length x)
       in ( if multiplier == "1"
              then ""
              else multiplier
          )
            ++ [head x]
