module Digits where

characters :: [Char]
characters = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\"#$%&'()*+,-./:;<=>?@[\\]^_`{}~"

newtype DigitalRepresentation = DR ([Int], Int)

instance Show DigitalRepresentation where
  show :: DigitalRepresentation -> String
  show (DR ([], _)) = "0"
  show (DR (digits, _)) = collapse (trim (go digits))
    where
      go [] = ""
      go (d : ds)
        | d < length characters = characters !! d : go ds
        | otherwise = '|' : show d ++ "|" ++ go ds

      trim = dropWhile (== '|') . reverse . dropWhile (== '|') . reverse
      collapse [] = []
      collapse ('|' : '|' : xs) = collapse ('|' : xs)
      collapse (x : xs) = x : collapse xs

digitsFromIntegral :: (Integral a) => Int -> a -> DigitalRepresentation
digitsFromIntegral base value
  | value < fromIntegral base = DR ([fromIntegral value], 1)
  | otherwise = DR (digits' ++ [fromIntegral remainder], length' + 1)
  where
    (digit, remainder) = value `quotRem` fromIntegral base
    DR (digits', length') = digitsFromIntegral base digit

digitsToIntegral :: (Integral a) => Int -> DigitalRepresentation -> a
digitsToIntegral base (DR (digits, len)) = go digits startPow
  where
    startPow = fromIntegral base ^ (len - 1)
    go [] _ = 0
    go (d : ds) pow = fromIntegral d * pow + go ds (pow `div` fromIntegral base)

reversedDigitsToIntegral :: (Integral a) => Int -> DigitalRepresentation -> a
reversedDigitsToIntegral base (DR (digits, len)) = go digits 1
  where
    go [] _ = 0
    go (d : ds) pow = fromIntegral d * pow + go ds (pow * fromIntegral base)

countDigits :: (Integral a) => Int -> a -> Int
countDigits base value
  | value >= fromIntegral base = 1 + countDigits base (value `div` fromIntegral base)
  | otherwise = 0

main :: IO ()
main = print (digitsFromIntegral 10 312)
