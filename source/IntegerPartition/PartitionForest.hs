{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module IntegerPartition.PartitionForest (PartitionForest (PartitionForest)) where

import IntegerPartition.PartitionTree (PartitionTree (..))

data PartitionForest a = PartitionForest [PartitionTree a]

instance (Show a) => Show (PartitionForest a) where
  show :: (Show a) => PartitionForest a -> String
  show (PartitionForest xs) = concatMapWithIndex showTree xs
    where
      showTree i t = go "" t connector
        where
          n = length xs
          connector
            | i == 0 && n == 1 = "└─" -- single tree
            | i == 0 = "┌─" -- first tree
            | i == n - 1 = "└─" -- last tree
            | otherwise = "├─" -- middle tree
      go prefix (PartitionNode x children) connector =
        prefix
          ++ connector
          ++ show x
          ++ "\n"
          ++ concatMapWithLast go2 children
        where
          newPrefix
            | connector == "└─" = prefix ++ "  "
            | otherwise = prefix ++ "│ "
          go2 child True = go newPrefix child "└─"
          go2 child False = go newPrefix child "├─"

      concatMapWithLast :: (PartitionTree a -> Bool -> String) -> [PartitionTree a] -> String
      concatMapWithLast _ [] = ""
      concatMapWithLast f [x] = f x True
      concatMapWithLast f (x : xs) = f x False ++ concatMapWithLast f xs

      concatMapWithIndex :: (Int -> PartitionTree a -> String) -> [PartitionTree a] -> String
      concatMapWithIndex f xs = concatMap (uncurry f) (zip [0 ..] xs)