module IsbnVerifier (isbn) where

isbn :: String -> Bool
isbn input =
  length isbnWithoutDashes == 10
    && checkChars isbnWithoutDashes
    && multiply isbnWithoutDashes 10 `mod` 11 == 0
  where
    isbnWithoutDashes = filter (/= '-') input

checkChars :: String -> Bool
checkChars [] = True
checkChars [x]
  | x `elem` ['0' .. '9'] ++ ['X'] = True
  | otherwise = False
checkChars (x : xs) = x `elem` ['0' .. '9'] && checkChars xs

multiply :: String -> Int -> Int
multiply [] _ = 0
multiply (x : xs) multiplier
  | x == 'X' = 10 * multiplier + rest
  | otherwise = (read [x] :: Int) * multiplier + rest
  where
    rest = multiply xs (multiplier - 1)
