module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n < 0 || n > 3000 = Nothing
  | otherwise = Just (convert (fromInteger n) 0)

convert :: Int -> Int -> String
convert _ 13 = ""
convert n index =
  if multiplier > 0
    then concat (replicate multiplier snd') ++ rest
    else "" ++ rest
  where
    multiplier = n `div` fst'
    fst' = fst (romanNums !! index)
    snd' = snd (romanNums !! index)
    rest = convert (n - fst' * multiplier) (index + 1)

romanNums :: [(Int, String)]
romanNums =
  [ (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I")
  ]
