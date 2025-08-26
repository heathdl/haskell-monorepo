module RaiseToPowerModulo where

import Prime (eulersTotient)

raiseToSomePowerModulo :: Int -> Int -> Int -> Int
raiseToSomePowerModulo _ 0 _ = 1
raiseToSomePowerModulo base value n = go (base `mod` n) value
  where
    go base value
      | odd value = (base * subsequent) `mod` n
      | otherwise = subsequent
      where
        subsequent = raiseToSomePowerModulo (base * base `mod` n) (value `div` 2) n

main :: IO ()
main = do
  print (raiseToSomePowerModulo a m n)
  where
    a = 71273612313
    m = 91137987423 `mod` eulersTotient n
    n = 1000