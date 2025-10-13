module Sequence.HappyNumbers where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (foldl')

isHappy :: Int -> Bool
isHappy x = go x (IntSet.singleton x)
  where
    go 1 visited = True
    go x visited
      | IntSet.member x' visited = False
      | otherwise = go x' (IntSet.insert x' visited)
      where
        x' = sumDigitsSquared x

-- https://oeis.org/A007770
happyNumbers :: [Int]
-- https://oeis.org/A031177
unhappyNumbers :: [Int]
(happyNumbers, unhappyNumbers) = go (IntSet.singleton 1) [2 ..]
  where
    go happySet (x : xs) =
      case inner x IntSet.empty of
        (True, visited) -> (x : hs, us)
          where
            (hs, us) = go (IntSet.union happySet visited) xs
        (False, visited) -> (hs, x : us)
          where
            (hs, us) = go happySet xs
      where
        inner n visited
          | IntSet.member n happySet = (True, visited)
          | n < x = (False, visited)
          | IntSet.member n visited = (False, visited)
          | otherwise = inner n' (IntSet.insert n visited)
          where
            n' = sumDigitsSquared n

-- https://oeis.org/A103369
happyNumberTerminals :: [Int]
happyNumberTerminals = go IntMap.empty [1 ..]
  where
    go !cache (x : xs) = t : go cache' xs
      where
        (t, cache') = case IntMap.lookup x cache of
          Just t -> (t, cache)
          Nothing -> inner x cache IntMap.empty []

    inner 1 cache visited path = (1, foldl' (\m v -> IntMap.insert v 1 m) (IntMap.insert 1 1 cache) path)
    inner n cache visited path
      | Just t <- IntMap.lookup n cache = (t, foldl' (\m v -> IntMap.insert v t m) cache path)
      | Just i <- IntMap.lookup n visited =
          let (prefix, cycleMembers) = splitAt i path
              entry = head cycleMembers
              cache1 = foldl' (\m v -> IntMap.insert v v m) cache cycleMembers
              cache2 = foldl' (\m v -> IntMap.insert v entry m) cache1 prefix
           in (entry, cache2)
      | otherwise =
          let visited' = IntMap.insert n (length path) visited
              path' = path ++ [n]
              n' = sumDigitsSquared n
           in inner n' cache visited' path'

sumDigitsSquared :: (Integral a) => a -> a
sumDigitsSquared 0 = 0
sumDigitsSquared n = r * r + sumDigitsSquared q
  where
    (q, r) = n `quotRem` 10

main :: IO ()
main = do
  mapM_ (\(a, b) -> putStrLn (show a ++ " " ++ show b)) (zip [1 ..] happyNumbers)
  print (take 20 happyNumberTerminals)