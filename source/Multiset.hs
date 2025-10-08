module Multiset (Multiset, fromSortedList, subsetsOfSize, cardinalityOfMultiset, canonicalisePartitions, computePartitions, computeCanonicalPartitions, multiSetDoubleMap, formatMultiSet) where

import Data.List (intercalate, sort)
import Data.Set qualified as Set
import IntegerPartition (integerPartitionTree)
import IntegerPartition.PartitionForest (PartitionForest (PartitionForest))
import IntegerPartition.PartitionTree (PartitionTree (PartitionNode))

type Multiset a = [(a, Int)]

fromSortedList :: (Eq a) => [a] -> Multiset a
fromSortedList = foldr step []
  where
    step x [] = [(x, 1)]
    step x ((y, n) : rest)
      | x == y = (y, n + 1) : rest
      | otherwise = (x, 1) : (y, n) : rest

subsetsOfSize :: Int -> Multiset a -> [(Multiset a, Multiset a)]
subsetsOfSize n _
  | n < 0 = []
subsetsOfSize 0 ms = [([], ms)]
subsetsOfSize _ [] = []
subsetsOfSize n ((x, c) : xs) =
  concatMap chooseK [0 .. min c n]
  where
    chooseK k = map insertInto (subsetsOfSize (n - k) xs)
      where
        subsetPrefix = [(x, k) | k /= 0]
        leftoverPrefix = [(x, c - k) | (c - k) /= 0]
        insertInto (s, l) = (subsetPrefix ++ s, leftoverPrefix ++ l)

cardinalityOfMultiset :: Multiset a -> Int
cardinalityOfMultiset = foldr ((+) . snd) 0

canonicalisePartitions :: (Ord a) => [[Multiset a]] -> [[Multiset a]]
canonicalisePartitions = Set.toList . Set.fromList . map sort

computeCanonicalPartitions :: (Ord a) => Multiset a -> [[Multiset a]]
computeCanonicalPartitions xs = concatMap (go Nothing xs) partitionSizes
  where
    PartitionForest partitionSizes = integerPartitionTree (cardinalityOfMultiset xs)

    go :: (Ord a) => Maybe (Int, [(a, Int)]) -> Multiset a -> PartitionTree Int -> [[Multiset a]]
    go m' ms (PartitionNode p children) =
      concatMap choose (filter (isCanonicalSubset m' . fst) (subsetsOfSize p ms))
      where
        choose (subset, leftover)
          | null children = [[subset] | null leftover]
          | otherwise =
              concatMap
                (map (subset :) . go (Just (p, subset)) leftover)
                children
        isCanonicalSubset Nothing _ = True
        isCanonicalSubset (Just (p', k')) set
          | p' == p = k' <= set
          | otherwise = True

computePartitions :: Multiset a -> [[Multiset a]]
computePartitions xs = concatMap (go xs) partitionSizes
  where
    go :: Multiset a -> PartitionTree Int -> [[Multiset a]]
    go ms (PartitionNode p c) = concatMap handleChoice asd
      where
        asd = subsetsOfSize p ms
        handleChoice (subset, leftover)
          | null c = [[subset] | null leftover]
          | otherwise = concatMap (map (subset :) . go leftover) c

    cardinality = cardinalityOfMultiset xs
    PartitionForest partitionSizes = integerPartitionTree cardinality

multiSetDoubleMap :: (Integral a) => (a -> Int -> a) -> ([a] -> c) -> Multiset a -> c
multiSetDoubleMap f g = g . map (uncurry f)

formatMultiSet :: (Show a) => Multiset a -> String
formatMultiSet xs =
  case parts of
    [] -> "1"
    _ -> intercalate " * " parts
  where
    parts = [showElem e n | (e, n) <- xs, n /= 0]
    showElem e 1 = show e
    showElem e n = show e ++ "^" ++ show n

main :: IO ()
main = do
  -- mapM_ (putStrLn . intercalate " | " . map formatMultiSet) (computePartitions d)
  mapM_ (print . map (product . map (uncurry (^)))) (canonicalisePartitions (computePartitions d))
  where
    d = [(2, 1), (3, 2), (5, 3)] :: Multiset Integer
