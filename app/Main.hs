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
import Text.Megaparsec (parse)

data Some = Error

main :: IO ()
main =
  runInputT defaultSettings loop
 where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "yaq) "
    case minput of
      Nothing -> return ()
      Just "quit" -> return ()
      Just "" -> loop
      Just input -> do
        let expr = parse parseq "" (pack input)
         in case expr of
              Left e -> outputStrLn ("Error: " <> show e)
              Right r -> outputStrLn $ show $ evalq r
        loop

-- yaq)
-- yaq)a:42
-- yaq)a :42
-- yaq)a: 42
-- yaq)a : 42
-- yaq)a
-- yaq)c
-- c is not defined