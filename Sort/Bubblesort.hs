module Sort.Bubblesort where

bubblesort :: (a -> a -> Bool) -> [a] -> [a]
bubblesort compare list = go (length list) list
  where
    go 0 list = list
    go endIndex list =
      let (list', swapped) = iteration compare endIndex list
       in if swapped
            then go (endIndex - 1) list'
            else list'

    iteration _ 0 list = (list, False)
    iteration _ _ [] = ([], False)
    iteration _ _ [x] = ([x], False)
    iteration compare endIndex (x : y : xs)
      | compare x y =
          let (tail, swapped) = iteration compare (endIndex - 1) (y : xs)
           in (x : tail, swapped)
      | otherwise =
          let (tail, _) = iteration compare (endIndex - 1) (x : xs)
           in (y : tail, True)