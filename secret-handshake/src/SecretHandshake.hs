module SecretHandshake (handshake) where

import Data.Bits ((.&.))

handshake :: Int -> [String]
handshake n
  | reverse' = reverse actions
  | otherwise = actions
  where
    wink = ["wink" | n .&. 1 /= 0]
    doubleBlink = ["double blink" | n .&. 2 /= 0]
    closeYourEyes = ["close your eyes" | n .&. 4 /= 0]
    jump = ["jump" | n .&. 8 /= 0]
    reverse' = n .&. 16 /= 0
    actions = wink ++ doubleBlink ++ closeYourEyes ++ jump
