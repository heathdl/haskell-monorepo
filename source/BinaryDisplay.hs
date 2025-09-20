module BinaryDisplay (showBinary, showBinaryList, showJustifiedBinaryList, showAutoJustifiedBinaryList) where

import BinaryDisplay.DisplayTypes (BinaryPairColouring, blueColour, greenColour, magentaColour, monochrome, monochromeInverted, redColour, yellowColour)
import Data.List (intercalate, transpose)

countBits :: (Integral a) => a -> Int
countBits 0 = 1
countBits x = subsequent + 1
  where
    remainder = x `div` 2
    subsequent
      | remainder > 0 = countBits remainder
      | otherwise = 0

extractBits :: (Integral a) => a -> [Bool]
extractBits 0 = [False]
extractBits x
  | even x = subsequent ++ [False]
  | otherwise = subsequent ++ [True]
  where
    remainder = x `div` 2
    subsequent
      | remainder > 0 = extractBits remainder
      | otherwise = []

leftPadBitArray :: Int -> [Bool] -> [Bool]
leftPadBitArray p xs = replicate (p - length xs) False ++ xs

showBinary :: (Integral a) => a -> String
showBinary 0 = ""
showBinary x
  | even x = subsequent ++ "  "
  | otherwise = subsequent ++ "██"
  where
    subsequent = showBinary (x `div` 2)

showBinaryListWithMapping :: (Integral a) => (a -> [Bool]) -> BinaryPairColouring -> [a] -> String
showBinaryListWithMapping integralToBits colouring xs = case xs of
  [] -> ""
  [a] -> showPair (integralToBits a) []
  [a, b] -> showPair (integralToBits a) (integralToBits b)
  (a : b : rest) -> showPair (integralToBits a) (integralToBits b) ++ "\n" ++ showBinaryListWithMapping integralToBits colouring rest
  where
    showPair as bs = concatMap colouring (maybeZip as bs)

    maybeZip [] [] = []
    maybeZip xs [] = map (\x -> (Just x, Nothing)) xs
    maybeZip [] ys = map (\y -> (Nothing, Just y)) ys
    maybeZip (x : xs) (y : ys) = (Just x, Just y) : maybeZip xs ys

showBinaryList :: (Integral a) => BinaryPairColouring -> [a] -> String
showBinaryList = showBinaryListWithMapping extractBits

showJustifiedBinaryList :: (Integral a) => BinaryPairColouring -> Int -> [a] -> String
showJustifiedBinaryList colouring j = showBinaryListWithMapping (leftPadBitArray j . extractBits) colouring

showAutoJustifiedBinaryList :: (Integral a) => BinaryPairColouring -> [a] -> String
showAutoJustifiedBinaryList colouring xs = showBinaryListWithMapping bitExtraction colouring xs
  where
    bitExtraction = leftPadBitArray paddingSize . extractBits
    paddingSize = maximum (map countBits xs)

main :: IO ()
main = do
  putStr (combineStrings (map (\x -> showAutoJustifiedBinaryList x [0 .. 31]) xs))
  where
    xs = [redColour, blueColour, greenColour, magentaColour, monochrome, monochromeInverted]
    combineStrings = unlines . map unwords . transpose . map lines