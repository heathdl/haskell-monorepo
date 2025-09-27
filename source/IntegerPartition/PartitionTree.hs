module IntegerPartition.PartitionTree (PartitionTree (PartitionNode)) where

data PartitionTree a = PartitionNode a [PartitionTree a]

instance (Show a) => Show (PartitionTree a) where
  show :: (Show a) => PartitionTree a -> String
  show t = go "" t True True
    where
      go :: String -> PartitionTree a -> Bool -> Bool -> String
      go prefix (PartitionNode x children) isLast isRoot =
        (if isRoot then "" else prefix ++ (if isLast then "└─" else "├─"))
          ++ show x
          ++ "\n"
          ++ concatMapWithLast (\child lastChild -> go (prefix ++ (if isLast then "" else "│ ")) child lastChild False) children

      concatMapWithLast :: (PartitionTree a -> Bool -> String) -> [PartitionTree a] -> String
      concatMapWithLast f [] = ""
      concatMapWithLast f [x] = f x True
      concatMapWithLast f (x : xs) = f x False ++ concatMapWithLast f xs
