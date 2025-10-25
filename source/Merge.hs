module Merge (mergeByWith, mergeBy, merge, mergeWith, mergeByWithDuplicates, mergeAllBy, mergeAll, mergeMonotonicByWith, mergeStrictlyIncreasing) where

import Data.List (groupBy, sortBy)
import Data.Ord (comparing)

mergeByWith :: (a -> a -> Ordering) -> ([a] -> b) -> [a] -> [a] -> [b]
mergeByWith cmp mrg = go
  where
    go [] ys = map mrg (groupEq ys)
    go xs [] = map mrg (groupEq xs)
    go xs@(x : xs') ys@(y : ys') =
      case cmp x y of
        LT ->
          let (eqs, rest) = spanEq y xs'
           in mrg (x : eqs) : go rest ys
        EQ ->
          let (eqsX, restX) = spanEq x xs'
              (eqsY, restY) = spanEq x ys'
           in mrg (x : y : eqsX ++ eqsY) : go restX restY
        GT ->
          let (eqs, rest) = spanEq x ys'
           in mrg (y : eqs) : go xs rest

    groupEq = groupBy (\a b -> cmp a b == EQ)
    spanEq ref = span (\a -> cmp a ref == EQ)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp xs@(x : xt) ys@(y : yt) =
  case cmp x y of
    LT -> x : mergeBy cmp xt ys
    GT -> y : mergeBy cmp xs yt
    EQ -> x : mergeBy cmp xt yt
mergeBy _ [] ys = ys
mergeBy _ xs [] = xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge = mergeBy compare

mergeWith :: (Ord a) => ([a] -> b) -> [a] -> [a] -> [b]
mergeWith = mergeByWith compare

mergeByWithDuplicates :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeByWithDuplicates cmp xs ys = concat (mergeByWith cmp id xs ys)

mergeAllBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergeAllBy cmp = foldr (mergeBy cmp) []

mergeAll :: (Ord a) => [[a]] -> [a]
mergeAll = mergeAllBy compare

mergeMonotonicByWith :: (a -> a -> Ordering) -> ([a] -> b) -> [[a]] -> [b]
mergeMonotonicByWith cmp mrg list =
  case list of
    [l] -> retroactivelyMerge l
    (frontier : untapped@((threshold : _) : _)) -> go [frontier] (filter (not . null) untapped) threshold
    _ -> []
  where
    retroactivelyMerge xs = map mrg (groupBy (\a b -> cmp a b == EQ) xs)

    go frontier [] _ = retroactivelyMerge (mergeAllBy cmp frontier)
    go [frontier] untapped@(tapped : untapped') threshold = retroactivelyMerge leadingMinima ++ go (tapped : [suffix]) untapped' threshold'
      where
        (leadingMinima, suffix) = span (\x -> cmp x threshold == LT) frontier
        threshold' = head (head untapped')
    go frontier untapped@(tapped : untapped') threshold =
      case extractMinima threshold frontier of
        (Just minima, remaining) -> mrg minima : go remaining untapped threshold
        (Nothing, _) -> go frontier' untapped' threshold'
          where
            frontier' = tapped : frontier
            threshold' = head (head untapped')

    extractMinima _ [] = (Nothing, [])
    extractMinima threshold rest@(first : _)
      | cmp minima threshold == LT = (Just allMinima, merged)
      | otherwise = (Nothing, rest)
      where
        minima = head first

        (block, suffix) = span isHeadMinima rest
        separatedBlock = map (span (\x -> cmp minima x == EQ)) block
        (allMinima, advancedBlock) = (concat as, bs)
          where
            (as, bs) = unzip separatedBlock

        sortedBlock = sortBy compareHeads advancedBlock
        merged = mergeBy compareHeads sortedBlock suffix

        compareHeads (a : _) (b : _) = cmp a b
        isHeadMinima (h : _) = cmp minima h == EQ

mergeStrictlyIncreasing :: (Ord a) => [[a]] -> [a]
mergeStrictlyIncreasing list =
  case list of
    [l] -> l
    (frontier : untapped@((threshold : _) : _)) -> go [frontier] (filter (not . null) untapped) threshold
    _ -> []
  where
    go frontier [] _ = mergeAll frontier
    go [frontier] untapped@(tapped : untapped') threshold = leadingMinima ++ go (tapped : [suffix]) untapped' threshold'
      where
        (leadingMinima, suffix) = span (< threshold) frontier
        threshold' = head (head untapped')
    go frontier untapped@(tapped : untapped') threshold =
      case extractMinima threshold frontier of
        (Just minima, remaining) -> minima : go remaining untapped threshold
        (Nothing, _) -> go frontier' untapped' threshold'
          where
            frontier' = tapped : frontier
            threshold' = head (head untapped')

    extractMinima _ [] = (Nothing, [])
    extractMinima threshold lists =
      case dropWhile null lists of
        [] -> (Nothing, lists)
        rest@(first : _)
          | minima < threshold -> (Just minima, merged)
          | otherwise -> (Nothing, rest)
          where
            minima = head first

            (block, suffix) = span (\(x : _) -> x == minima) rest
            sortedBlock = sortBy (comparing head) (map tail block)
            merged = mergeBy (comparing head) sortedBlock suffix
