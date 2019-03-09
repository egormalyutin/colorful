{-# LANGUAGE MultiWayIf #-}

module System.Console.Colorful.Internal where

import qualified Data.Text as T
import Data.Bits

-- | Replace substring.
replace :: String -> String -> String -> String
replace str find repl = T.unpack . T.replace (T.pack find) (T.pack repl) $ T.pack str

-- | Convert code to ANSI.
toANSI :: String -> String
toANSI s = "\ESC[" <> s <> "m"

-- | Colorize string.
colorize :: String -> String -> String -> String
colorize l r s = open <> (replace s close open) <> close
    where open  = toANSI l
          close = toANSI r

-- | Divide two Ints and get rounded Int.
roundDiv :: Int -> Int -> Int
roundDiv x y = round $ (fromIntegral x) / (fromIntegral y)

-- | Divide two Ints and get Float.
divInts :: Int -> Int -> Float
divInts x y = (fromIntegral x) / (fromIntegral y)

type RGB = (Int, Int, Int)
type ANSI16 = Int
type ANSI256 = Int

-- | Convert RGB to ansi16.
rgbToAnsi16 :: RGB -> ANSI16
rgbToAnsi16 (r, g, b) = case v of
        0 -> 30
        2 -> ansi + 60
        _ -> ansi

    where
        v = maximum [divInts r 255, divInts g 255, divInts b 255] * 2
        ansi = (30 +
            ((shiftL (roundDiv b 255) 2) .|.
             (shiftL (roundDiv g 255) 1) .|.
             (roundDiv r 255)))

-- | Convert RGB to ansi256.
rgbToAnsi256 :: RGB -> ANSI256
rgbToAnsi256 (r, g, b) = if r == g && g == b
    then if | r < 8     -> 16
            | r > 248   -> 231
            | otherwise -> (round $ ((fromIntegral $ r - 8) / (fromIntegral 247)) * 24) + 232

    else (16 +
         (36 * (roundDiv r 51)) +
         (6 * (roundDiv g 51)) +
         (roundDiv b 51))

-- | Convert ansi256 to RGB.
ansi256ToRGB :: ANSI256 -> RGB
ansi256ToRGB a' = if a' >= 232
        then (c, c, c)
        else (r, g, b)

    where
        c = (a' - 232) * 10 + 8

        a = a' - 16
        rem = mod a 36
        r = (div a 36) * 51
        g = (div rem 6) * 51
        b = (mod rem 6) * 51

-- | Convert ansi256 to ansi16.
ansi256ToAnsi16 :: ANSI256 -> ANSI16
ansi256ToAnsi16 = rgbToAnsi16 . ansi256ToRGB
