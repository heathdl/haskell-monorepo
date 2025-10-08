module Sort.BubbleSort where

bubbleSort :: (a -> a -> Bool) -> [a] -> [a]
bubbleSort compare list = go (length list) list
  where
    go 0 list = list
    go endIndex list
      | swap' = go (endIndex - 1) list'
      | otherwise = list'
      where
        (list', swap') = iteration compare endIndex list

    iteration _ 0 list = (list, False)
    iteration _ _ [] = ([], False)
    iteration _ _ [x] = ([x], False)
    iteration compare endIndex (x : y : xs)
      | swap = (x : tail, swap')
      | otherwise = (y : tail, True)
      where
        swap = compare x y
        (tail, swap')
          | swap = iteration compare (endIndex - 1) (y : xs)
          | otherwise = iteration compare (endIndex - 1) (x : xs)
