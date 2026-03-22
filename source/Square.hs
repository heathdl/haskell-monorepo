module Square (integerSquareRoot, isPerfectSquare, newtonRaphsonSquareRoot) where

import Data.List (scanl')
import Data.Maybe (fromJust)
import Debug.Trace (trace)

integerSquareRoot :: (Integral a) => a -> a
integerSquareRoot value = go value
  where
    go x
      | x' == x = x'
      | otherwise = go x'
      where
        x' = (x * x + value) `div` (2 * x)

isPerfectSquare :: (Integral a) => a -> Bool
isPerfectSquare value = root * root == value
  where
    root = integerSquareRoot value

newtonRaphsonSquareRoot :: Double -> Double
newtonRaphsonSquareRoot value = go value
  where
    go x
      | x == x' = x
      | otherwise = go x'
      where
        x' = 0.5 * (x + value / x)

-- `squareRoots !! n` is the square root of n, equivalent precision to `map newtonRaphsonSquareRoot [1..]`
-- Uses a recurrence to predict the next square root, then applies Newton-Raphson, changing how many steps
-- are used based on how far into the sequence we are (the predictor gets more accurate n→∞).

squareRoots :: [Double]
squareRoots = go 1 1 3
  where
    go !r !sq !sq' = scanl' step r [sq + 1 .. sq'] ++ go (r + 1) (sq' + 1) (sq' + 2 * r + 3)
    step !a !x
      | a > 138 = newton1
      | a > 6 = newton2
      | a > 2 = newton3
      | otherwise = newton4
      where
        predictor = (x - 0.5) / a -- sqrt(x) ~ sqrt(x - 1) + 1 / 2sqrt(x - 1) = (2x - 1) / 2sqrt(x - 1) = (x - 0.5) / sqrt(x - 1)
        newton1 = 0.5 * (predictor + x / predictor)
        newton2 = 0.5 * (newton1 + x / newton1)
        newton3 = 0.5 * (newton2 + x / newton2)
        newton4 = 0.5 * (newton3 + x / newton3)

-- `approximateSquareRoots !! n` is asymptotically the square root of n.
-- After ~8192 terms this is equal to `squareRoots` with no error.
-- Uses the same predictor as `squareRoots` but applies only one step of Newton-Raphson

approximateSquareRoots :: [Double]
approximateSquareRoots = go 1 1 3
  where
    go !r !sq !sq' = scanl' step r [sq + 1 .. sq'] ++ go (r + 1) (sq' + 1) (sq' + 2 * r + 3)
    step !a !x = 0.5 * (a' + x / a')
      where
        a' = (x - 0.5) / a -- sqrt(x) ~ sqrt(x - 1) + 1 / 2sqrt(x - 1) = (2x - 1) / 2sqrt(x - 1) = (x - 0.5) / sqrt(x - 1)

main :: IO ()
main = do
  mapM_ (\(a, b) -> putStrLn (show a ++ "," ++ show b)) (zip [1 .. 8192] (map (^ 2) squareRoots))