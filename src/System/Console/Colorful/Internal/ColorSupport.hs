{-# LANGUAGE MultiWayIf #-}

module System.Console.Colorful.Internal.ColorSupport where

import System.Environment
import GHC.IO.Unsafe
import Text.Read
import Control.Applicative

type ColorSupport = Int

hasFlags :: [String] -> [String] -> Bool
hasFlags flags = any (`elem` ([('-':), (\x -> '-':'-':x)] <*> flags))

getColorSupport :: IO ColorSupport
getColorSupport = do
    argv <- getArgs

    forceColor <- if | hasFlags ["no-color", "no-colors", "color=false", "color=never"] argv -> return $ Just 0
                     | hasFlags ["color", "colors", "color=true", "color=always"] argv -> return $ Just 1
                     | otherwise -> do
                         forceEnv <- lookupEnv "FORCE_COLOR"

                         case forceEnv of
                             Just "true" -> return $ Just 1
                             Just "false" -> return $ Just 0
                             Just str -> case readMaybe str of
                                 Just x -> return . Just $ min x 3
                                 Nothing -> return $ Just 1

                             Nothing -> return Nothing

    case forceColor of
        Just x -> return x
        Nothing -> return 1

-- | Color support level (it's just a "unsafePerformIO getColorSupport")
colorSupport :: ColorSupport
{-# NOINLINE colorSupport #-}
colorSupport = unsafePerformIO getColorSupport
