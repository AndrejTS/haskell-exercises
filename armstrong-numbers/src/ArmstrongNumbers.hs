module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong num = num == sum (map (^ length digits) digits)
  where 
    digits = digs num

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]
