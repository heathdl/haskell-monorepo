module IntegerPartition (integerPartition, integerPartitionTree, subsetSumTree) where

-- All sets $S$ such that $\sum_{i=0}{n}S_{i} = n$
-- https://en.wikipedia.org/wiki/Integer_partition

import IntegerPartition.PartitionForest (PartitionForest (PartitionForest))
import IntegerPartition.PartitionTree (PartitionTree (PartitionNode))

integerPartition :: (Integral a) => a -> [[a]]
integerPartition n
  | n < 0 = []
  | n == 0 = [[]]
  | otherwise = reverse (map reverse (go n n))
  where
    go m k
      | m == 0 = [[]]
      | k <= 0 = []
      | otherwise = map (k :) (go (m - k) (min (m - k) k)) ++ go m (k - 1)

integerPartitionTree :: (Integral a) => a -> PartitionForest a
integerPartitionTree n
  | n <= 0 = PartitionForest []
  | otherwise = PartitionForest (build n 1)
  where
    build m low
      | m == 0 = []
      | otherwise =
          [ PartitionNode p children
            | p <- [low .. m],
              let children = build (m - p) p,
              not (null children) || m - p == 0
          ]

subsetSumTree :: (Integral a, Ord a) => [a] -> a -> PartitionForest a
subsetSumTree xs n
  | n <= 0 = PartitionForest []
  | otherwise = PartitionForest (build n xs)
  where
    build m ys
      | m == 0 = []
      | otherwise =
          let available = dropWhile (> m) ys
           in [ PartitionNode p children
                | p <- available,
                  let rest = dropWhile (> p) available,
                  let children = build (m - p) rest,
                  not (null children) || m - p == 0
              ]

main :: IO ()
main = print (subsetSumTree (map (^ 2) [9, 8 .. 1]) 10)
