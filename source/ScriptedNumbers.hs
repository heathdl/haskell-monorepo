module ScriptedNumbers (toSubscript, toSuperscript, toRatio) where

subscriptDigits :: String
subscriptDigits = "₀₁₂₃₄₅₆₇₈₉"

superscriptDigits :: String
superscriptDigits = "⁰¹²³⁴⁵⁶⁷⁸⁹"

toScript :: (Integral a) => String -> a -> String
toScript script n = reverse (go n)
  where
    go 0 = ""
    go x = (script !! fromIntegral r) : go q
      where
        (q, r) = x `quotRem` 10

toSubscript :: (Integral a) => a -> String
toSubscript = toScript subscriptDigits

toSuperscript :: (Integral a) => a -> String
toSuperscript = toScript superscriptDigits

toRatio :: (Integral a) => a -> a -> String
toRatio a b = toSuperscript a ++ "/" ++ toSubscript b

main :: IO ()
main = do
  putStrLn (toRatio 32 64)
  putStrLn (toSubscript 32)
  putStrLn (toSuperscript 64)
