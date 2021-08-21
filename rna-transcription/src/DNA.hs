module DNA (toRNA) where

mapper :: Char -> Char
mapper e
  | e == 'G' = 'C'
  | e == 'C' = 'G'
  | e == 'T' = 'A'
  | e == 'A' = 'U'
  | otherwise = error ""

toRNA :: String -> Either Char String
toRNA xs
  | not (null invalidChars) = Left (head invalidChars)
  | otherwise = Right (map mapper xs)
  where
    invalidChars = filter (\x -> x `notElem` ['A', 'C', 'G', 'T']) xs
