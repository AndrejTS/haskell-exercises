module Pangram (isPangram) where

import Data.Char


isPangram :: String -> Bool
isPangram text = all (`elem` lowercaseText) ['a'..'z']
                where lowercaseText = map toLower text
