module Merge (merge, mergeAll, mergeInfiniteMonotonic) where

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

-- `map head lists` must be monotonically increasing, and so should each individual list
mergeInfiniteMonotonic :: (Ord a) => [[a]] -> [a]
mergeInfiniteMonotonic [] = []
mergeInfiniteMonotonic [l] = l
mergeInfiniteMonotonic (l : ls) =
  case dropWhile null ls of
    [] -> l
    (next : rest) -> go [l] (next : rest) (head next)
  where
    go frontier [] _ = mergeAll frontier
    go frontier untapped@(tapped : untapped') threshold =
      case extractMinima threshold frontier of
        (Just minima, remaining) -> minima : go remaining (tapped : untapped) threshold
        (Nothing, _) ->
          case dropWhile null untapped' of
            [] -> mergeAll (tapped : frontier)
            ((threshold' : _) : _) -> go (tapped : frontier) untapped' threshold'

    extractMinima _ [] = (Nothing, [])
    extractMinima threshold lists
      | null heads = (Nothing, [])
      | minima >= threshold = (Nothing, lists)
      | otherwise = (Just minima, subsequent)
      where
        heads = [x | (x : _) <- lists]
        minima = minimum heads
        subsequent = [if h == minima then xs' else xs | xs@(h : xs') <- lists]

main :: IO ()
main = print (take 256 (mergeInfiniteMonotonic exampleSequence))
  where
    exampleSequence = [[10 ^ k + j * (10 ^ k - 1) `div` 9 | j <- [0 ..]] | k <- [2 ..]]
