module Sort.MergeSort where

mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort compare list =
  merge compare (mergeSort compare first) (mergeSort compare second)
  where
    half = length list `div` 2
    first = take half list
    second = drop half list

    merge _ [] y = y
    merge _ x [] = x
    merge compare (x : xTail) (y : yTail)
      | compare x y = x : merge compare xTail (y : yTail)
      | otherwise = y : merge compare (x : xTail) yTail