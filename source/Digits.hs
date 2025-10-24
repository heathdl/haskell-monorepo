module Digits where

import Data.List (group, sort)
import Merge (mergeAll)
import Multiset (Multiset, insertCount, orderedPermutations, removeAll)

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

instance Eq DigitalRepresentation where
  (==) :: DigitalRepresentation -> DigitalRepresentation -> Bool
  (DR (digits1, _)) == (DR (digits2, _)) = digits1 == digits2

instance Ord DigitalRepresentation where
  compare :: DigitalRepresentation -> DigitalRepresentation -> Ordering
  compare (DR (digits1, _)) (DR (digits2, _)) = compare digits1 digits2

digitsFromIntegral :: (Integral a) => Int -> a -> DigitalRepresentation
digitsFromIntegral base value
  | value < fromIntegral base = DR ([fromIntegral value], 1)
  | otherwise = DR (digits' ++ [fromIntegral remainder], length' + 1)
  where
    (digit, remainder) = value `quotRem` fromIntegral base
    DR (digits', length') = digitsFromIntegral base digit

digitsFromList :: (Integral a) => [a] -> DigitalRepresentation
digitsFromList xs = DR (map fromIntegral xs, length xs)

digitsToList :: (Integral a) => DigitalRepresentation -> [a]
digitsToList (DR (digits, _)) = map fromIntegral digits

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

digitsToOrderedMultiset :: (Integral a, Ord a) => DigitalRepresentation -> Multiset a
digitsToOrderedMultiset (DR (digits, _)) = foldr (\d acc -> insertCount (fromIntegral d) 1 acc) [] digits

digitReorderings :: (Integral a) => Multiset a -> [a]
digitReorderings x = map (digitsToIntegral 10) (concat (iterate (mergeAll . map insertZerosFor) initial))
  where
    digitMultiset = Multiset.removeAll 0 x
    initial = map digitsFromList (Multiset.orderedPermutations digitMultiset)

    insertZerosFor (DR (digits, len)) = [DR (insertAt digits i, len + 1) | (i, d) <- zip [1 ..] digits, d /= 0]
    insertAt ds i = a ++ 0 : b
      where
        (a, b) = splitAt i ds

countDigits :: (Integral a) => Int -> a -> Int
countDigits base value
  | value >= fromIntegral base = 1 + countDigits base (value `div` fromIntegral base)
  | otherwise = 0

main :: IO ()
main = print (digitsFromIntegral 10 312)
