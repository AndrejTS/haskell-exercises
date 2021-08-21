module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
  if any (\x -> x /= 'A' && x /= 'C' && x /= 'G' && x /= 'T') xs
    then Left "error"
    else Right (Map.fromList (map (count xs) [A, C, G, T]))

count :: String -> Nucleotide -> (Nucleotide, Int)
count s n = (n, length (filter (\x -> x == head (show n)) s))
