module Sort.InsertionSort where

insertionSort :: (a -> a -> Bool) -> [a] -> [a]
insertionSort _ [] = []
insertionSort _ [x] = [x]
insertionSort compare (x : xTail) = insertHead (insertionSort compare xTail)
  where
    insertHead [] = [x]
    insertHead (y : yTail)
      | compare x y = x : y : yTail
      | otherwise = y : insertHead yTail
