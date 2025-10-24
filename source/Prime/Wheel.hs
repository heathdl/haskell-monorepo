module Prime.Wheel where

import Prime (isPrime)
import Prime.Coprimes (calculateCoprimes)

wheeledPrimes :: (Integral a) => a -> [a]
wheeledPrimes wheel = filter isPrime spokes
  where
    residues = calculateCoprimes wheel
    spokes = [wheel * k + r | k <- [0 ..], r <- residues]

main :: IO ()
main = print (wheeledPrimes (2 * 3 * 5 * 7))