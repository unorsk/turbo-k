module Main where

import Data.Text (pack)
import Parserq (parseq)
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
    minput <- getInputLine "yaq> "
    case minput of
      Nothing -> return ()
      Just "quit" -> return ()
      Just input ->
        let r = parse parseq "" (pack input)
         in do
              outputStrLn $ show r
              -- outputStrLn $ "Input was: " ++ input
              loop

-- yaq>
-- q)a:42
-- q)a :42
-- q)a: 42
-- q)a : 42
-- q)a
-- q)c
-- c is not defined