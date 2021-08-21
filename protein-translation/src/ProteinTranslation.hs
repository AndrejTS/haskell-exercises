module ProteinTranslation (proteins) where

proteins :: String -> Maybe [String]
proteins rna = Just (map translate (threesome rna))

threesome :: String -> [String]
threesome (x : y : z : rest)
  | threesome' `elem` ["UAA", "UAG", "UGA"] = [] -- stop
  | otherwise = threesome' : threesome rest
  where
    threesome' = [x, y, z]
threesome _ = []

translate :: String -> String
translate str
  | str == "AUG" = "Methionine"
  | str == "UGG" = "Tryptophan"
  | str `elem` ["UUU", "UUC"] = "Phenylalanine"
  | str `elem` ["UUA", "UUG"] = "Leucine"
  | str `elem` ["UCU", "UCC", "UCA", "UCG"] = "Serine"
  | str `elem` ["UAU", "UAC"] = "Tyrosine"
  | str `elem` ["UGU", "UGC"] = "Cysteine"
  | otherwise = error "blalblalb"
