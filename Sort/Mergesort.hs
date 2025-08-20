module Sort.Mergesort where

mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort _ [] = []
mergesort _ [x] = [x]
mergesort compare list =
  merge compare (mergesort compare first) (mergesort compare second)
  where
    half = length list `div` 2
    first = take half list
    second = drop half list

    merge _ [] y = y
    merge _ x [] = x
    merge compare (x : xTail) (y : yTail)
      | compare x y = x : merge compare xTail (y : yTail)
      | otherwise = y : merge compare (x : xTail) yTail