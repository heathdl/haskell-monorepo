module Sequence.PerfectPowers where

import Merge (mergeStrictlyIncreasing)

perfectPowers :: (Integral a) => [a]
perfectPowers = mergeStrictlyIncreasing powers

powers :: (Integral a) => [[a]]
powers = [iterate (* k) (k * k) | k <- [2 ..]]

main :: IO ()
main =
  mapM_ (\(a, b) -> putStrLn (show a ++ " " ++ show b)) (zip [1 ..] smallPerfectPowers)
  where
    smallPerfectPowers = takeWhile (<= 100) perfectPowers
