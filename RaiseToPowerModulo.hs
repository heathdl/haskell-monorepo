module RaiseToPowerModulo where

import Prime (eulersTotient)

raiseToSomePowerModulo :: Int -> Int -> Int -> Int
raiseToSomePowerModulo _ 0 _ = 1
raiseToSomePowerModulo base value n = go (base `mod` n) value
  where
    go base value
      | remainder == 1 = (base * subsequent) `mod` n
      | otherwise = subsequent
      where
        (quotient, remainder) = value `quotRem` 2
        subsequent = raiseToSomePowerModulo (base * base `mod` n) quotient n

main :: IO ()
main = do
  print (raiseToSomePowerModulo a m n)
  where
    a = 71273612313
    m = 91137987423 `mod` eulersTotient n
    n = 1000