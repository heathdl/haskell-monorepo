module BinaryDisplay.DisplayTypes (BinaryPairColouring, fullColourSchema, monochromaticSchema, blueColour, redColour, greenColour, yellowColour, magentaColour, cyanColour, whiteColour, monochrome, monochromeInverted) where

type BinaryPairColouring = (Maybe Bool, Maybe Bool) -> String

reset :: String
reset = "\27[0m"

blueColour :: BinaryPairColouring
blueColour = fullColourSchema ("\ESC[34m", "\ESC[44m") ("\ESC[36m", "\ESC[46m")

redColour :: BinaryPairColouring
redColour = fullColourSchema ("\ESC[31m", "\ESC[41m") ("\ESC[91m", "\ESC[101m")

greenColour :: BinaryPairColouring
greenColour = fullColourSchema ("\ESC[32m", "\ESC[42m") ("\ESC[92m", "\ESC[102m")

yellowColour :: BinaryPairColouring
yellowColour = fullColourSchema ("\ESC[33m", "\ESC[43m") ("\ESC[93m", "\ESC[103m")

magentaColour :: BinaryPairColouring
magentaColour = fullColourSchema ("\ESC[35m", "\ESC[45m") ("\ESC[95m", "\ESC[105m")

cyanColour :: BinaryPairColouring
cyanColour = fullColourSchema ("\ESC[36m", "\ESC[46m") ("\ESC[96m", "\ESC[106m")

whiteColour :: BinaryPairColouring
whiteColour = fullColourSchema ("\ESC[37m", "\ESC[47m") ("\ESC[97m", "\ESC[107m")

fullColourSchema :: (String, String) -> (String, String) -> BinaryPairColouring
fullColourSchema (highForeground, highBackground) (lowForeground, lowBackground) pair = case pair of
  (Just a, Just b)
    | a && b -> highForeground ++ "█" ++ reset
    | a -> highForeground ++ lowBackground ++ "▀" ++ reset
    | b -> highForeground ++ lowBackground ++ "▄" ++ reset
    | otherwise -> lowBackground ++ " " ++ reset
  (Just a, Nothing)
    | a -> highForeground ++ "▀" ++ reset
    | otherwise -> lowForeground ++ "▀" ++ reset
  (Nothing, Just b)
    | b -> highForeground ++ "▄" ++ reset
    | otherwise -> lowForeground ++ "▄" ++ reset
  (Nothing, Nothing) -> " "

monochrome :: BinaryPairColouring
monochrome = monochromaticSchema "█" "▀" "▄" " "

monochromeInverted :: BinaryPairColouring
monochromeInverted = monochromaticSchema " " "▄" "▀" "█"

monochromaticSchema :: String -> String -> String -> String -> BinaryPairColouring
monochromaticSchema full top bottom none pair = case pair of
  (Just a, Just b)
    | a && b -> full
    | a -> top
    | b -> bottom
    | otherwise -> none
  (Just a, Nothing)
    | a -> top
    | otherwise -> none
  (Nothing, Just b)
    | b -> bottom
    | otherwise -> none
  (Nothing, Nothing) -> none

main :: IO ()
main = do
  putStrLn (concatMap blueColour variants)
  putStrLn (concatMap monochrome variants)
  putStrLn (concatMap monochromeInverted variants)
  where
    variants = [(x, y) | x <- [Just True, Just False, Nothing], y <- [Just True, Just False, Nothing]]