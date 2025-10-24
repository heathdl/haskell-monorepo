module Sequence.GrayCodes where

import BinaryDisplay (showAutoJustifiedBinaryList)
import BinaryDisplay.DisplayTypes (blueColour)

grayCodes :: (Integral a) => [a]
grayCodes = concat nBitGrayCodes

nBitGrayCodes :: (Integral a) => [[a]]
nBitGrayCodes = map firstOfThree (iterate extendGrayCodes ([0], [], 1))
  where
    firstOfThree (a, _, _) = a
    extendGrayCodes (grayCode, previous, n) = (grayCode', previous', n * 2)
      where
        grayCode' = map (n +) previous'
        previous' = reverse grayCode ++ previous

main :: IO ()
main = do
  putStrLn (showAutoJustifiedBinaryList blueColour (take 64 grayCodes))