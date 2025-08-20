module Sort.Quicksort where

import Data.List (partition)

quicksort :: (a -> a -> Bool) -> [a] -> [a]
quicksort _ [] = []
quicksort compare (pivot : list) =
  let (smaller, larger) = partition (`compare` pivot) list
   in quicksort compare smaller ++ [pivot] ++ quicksort compare larger