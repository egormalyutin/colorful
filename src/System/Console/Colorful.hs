module System.Console.Colorful
    ( module System.Console.Colorful
    , module System.Console.Colorful.Internal.ColorSupport
    ) where

import System.Console.Colorful.Internal
import System.Console.Colorful.Internal.ColorSupport
import System.Info

-- ** Change color of string
black, red, green, yellow, blue, magenta, cyan, gray, white, lightGray, darkGray, lightRed, lightGreen, lightYellow, lightBlue, lightMagenta, lightCyan :: String -> String

black   = colorize "30" "39"
red     = colorize "31" "39"
green   = colorize "32" "39"
yellow  = colorize "33" "39"

-- Normal blue color on windows is illegible
blue = if os == "windows"
    then colorize "34" "39"
    else colorize "94" "39"

magenta = colorize "35" "39"
cyan    = colorize "36" "39"
gray    = colorize "37" "39"
white   = colorize "97" "39"

lightGray    = colorize "37" "39"
darkGray     = colorize "90" "39"
lightRed     = colorize "91" "39"
lightGreen   = colorize "92" "39"
lightYellow  = colorize "93" "39"
lightBlue    = colorize "94" "39"
lightMagenta = colorize "95" "39"
lightCyan    = colorize "96" "39"

-- ** Change background color of string
bgBlack, bgRed, bgGreen, bgYellow, bgBlue, bgMagenta, bgCyan, bgWhite, bgLightGray, bgDarkGray, bgLightRed, bgLightGreen, bgLightYellow, bgLightBlue, bgLightMagenta, bgLightCyan :: String -> String

bgBlack   = colorize "40" "49"
bgRed     = colorize "41" "49"
bgGreen   = colorize "42" "49"
bgYellow  = colorize "43" "49"
bgBlue    = colorize "44" "49"
bgMagenta = colorize "45" "49"
bgCyan    = colorize "46" "49"
bgWhite   = colorize "106" "49"

bgLightGray    = colorize "47" "49"
bgDarkGray     = colorize "100" "49"
bgLightRed     = colorize "101" "49"
bgLightGreen   = colorize "102" "49"
bgLightYellow  = colorize "103" "49"
bgLightBlue    = colorize "104" "49"
bgLightMagenta = colorize "105" "49"
bgLightCyan    = colorize "106" "49"

-- ** Change style of string
bold, dim, underline, blink, reverseS, hide :: String -> String

bold      = colorize "1" "21"
dim       = colorize "2" "22"
underline = colorize "4" "24"
blink     = colorize "5" "25"
reverseS  = colorize "7" "27"
hide      = colorize "8" "28"

-- ** Other color changing utilities
color256, bgColor256 :: Int -> String -> String

color256   c = colorize ("38;5;" ++ show c) "39"
bgColor256 c = colorize ("48;5;" ++ show c) "39"


