module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []
discard p (x : xs) =
  if not (p x)
    then x : discard p xs
    else discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = [x | x <- xs, p x]
