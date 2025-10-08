module Sort
  ( bubbleSort,
    quicksort,
    mergeSort,
  )
where

import Sort.BubbleSort (bubbleSort)
import Sort.MergeSort (mergeSort)
import Sort.Quicksort (quicksort)
import System.Random (randomRIO)

generateRandomList :: Int -> IO [Int]
generateRandomList 0 = return []
generateRandomList n = do
  head <- randomRIO (0, 255)
  tail <- generateRandomList (n - 1)
  return (head : tail)

applySorts :: [(a -> a -> Bool) -> [a] -> [a]] -> (a -> a -> Bool) -> [a] -> [[a]]
applySorts sorts compare list = [sort compare list | sort <- sorts]

main :: IO ()
main = do
  input <- generateRandomList 16
  let results = applySorts sorts (<) input
      output = head results
  putStrLn ("Unsorted: " ++ show input)
  putStrLn ("Sorted: " ++ show output)
  putStrLn ("All implementations match: " ++ show (all (== head results) (tail results)))
  where
    sorts = [quicksort, bubbleSort, mergeSort]
