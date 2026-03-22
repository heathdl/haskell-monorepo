module Enumerations where

import Data.List (transpose)

integers :: (Integral a) => [a]
integers = 0 : go [1 ..] [-1, -2 ..]
  where
    go (x : xs) (y : ys) = x : y : go xs ys

gaussianIntegers :: (Integral a) => [(a, a)]
gaussianIntegers = cartesianProduct integers integers

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct [] _ = []
cartesianProduct xs ys = concat (tail (go [] [[(x, y) | x <- xs] | y <- ys]))
  where
    go b es_ =
      [h | h : _ <- b] : case es_ of
        [] -> transpose ts
        e : es -> go (e : ts) es
      where
        ts = [t | _ : t <- b]

main = print (take 5000 integers)