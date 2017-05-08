module Main where

import           Parser                   (parse)
import           System.Console.Haskeline
    ( InputT
    , defaultSettings
    , getInputLine
    , outputStrLn
    , runInputT
    )

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      i <- getInputLine "% "
      case i of
        Nothing     -> return ()
        Just "quit" -> return ()
        Just input  -> do
          outputStrLn $ case parse input of
            Nothing -> "Failed to parse input"
            Just e  -> "Input was: " ++ (show e)
          loop
