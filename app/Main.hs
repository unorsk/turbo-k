module Main where

import Data.Text (pack)
import Parserq (evalq, parseq)
import System.Console.Haskeline
  ( InputT
  , defaultSettings
  , getInputLine
  , outputStrLn
  , runInputT
  )
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main =
  runInputT defaultSettings loop
 where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "k) "
    case minput of
      Nothing -> return ()
      Just "quit" -> return ()
      Just "" -> loop
      Just input -> do
        let expr = parse parseq "" (pack input)
         in case expr of
              Left e -> outputStrLn $ errorBundlePretty e
              Right r -> outputStrLn $ show $ evalq r
        loop
