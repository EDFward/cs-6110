module Main where

import           System.Console.Haskeline
    ( InputT
    , defaultSettings
    , getInputLine
    , outputStrLn
    , runInputT
    )

import           Eval
import           Expr
import           Parser

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
          case parse input of
            Nothing -> outputStrLn $ "Failed to parse input"
            Just e  -> step Nothing e
          loop

step :: Maybe Expr -> Expr -> InputT IO ()
step Nothing expr = step (Just expr) (betaStepCBV expr)
step (Just prevExpr) currExpr
  | prevExpr == currExpr = outputStrLn $ "Result:\t" ++ (show currExpr)
  | otherwise            = do
      outputStrLn $ "Step:\t" ++ show prevExpr
      step (Just currExpr) (betaStepCBV currExpr)
