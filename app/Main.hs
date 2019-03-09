module Main where

import System.Console.Colorful
import System.Console.Colorful.Internal

main :: IO ()
main = do
    putStrLn . blink $ (color256 198 $ ("sdsds" ++ (underline . green $ bgLightRed "hi") ++ "iiii")) ++ (reverseS . hide $ green "olala")
    print $ colorSupport

