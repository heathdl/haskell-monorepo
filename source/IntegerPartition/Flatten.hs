module IntegerPartition.Flatten where

import IntegerPartition.PartitionForest (PartitionForest (PartitionForest))
import IntegerPartition.PartitionTree (PartitionTree (PartitionNode))
import Multiset (Multiset)

flattenTreeBranches :: (Ord a) => PartitionTree a -> [[(a, Int)]]
flattenTreeBranches (PartitionNode val []) = [[(val, 1)]]
flattenTreeBranches (PartitionNode val children) =
  [mergeVal val branch | child <- children, branch <- flattenTreeBranches child]
  where
    mergeVal v [] = [(v, 1)]
    mergeVal v ((x, c) : xs)
      | x == v = (x, c + 1) : xs
      | otherwise = (v, 1) : (x, c) : xs

flattenForestBranches :: (Ord a) => PartitionForest a -> [[(a, Int)]]
flattenForestBranches (PartitionForest trees) = concatMap flattenTreeBranches trees
