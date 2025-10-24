module Merge (merge, mergeAll) where

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x : xt) ys@(y : yt) =
  case compare x y of
    LT -> x : merge xt ys
    GT -> y : merge xs yt
    EQ -> x : merge xt yt
merge [] ys = ys
merge xs [] = xs

mergeAll :: (Ord a) => [[a]] -> [a]
mergeAll = foldr merge []