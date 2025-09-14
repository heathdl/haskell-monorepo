module Sort.MergeSort where

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] y = y
merge _ x [] = x
merge compare (x : xs) (y : ys)
  | compare x y = x : merge compare xs (y : ys)
  | otherwise = y : merge compare (x : xs) ys

mergeAll :: (a -> a -> Bool) -> [[a]] -> [a]
mergeAll _ [] = []
mergeAll _ [xs] = xs
mergeAll compare (xs : ys : rest) = mergeAll compare (merge compare xs ys : rest)

mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort compare list =
  merge compare (mergeSort compare first) (mergeSort compare second)
  where
    half = length list `div` 2
    first = take half list
    second = drop half list
