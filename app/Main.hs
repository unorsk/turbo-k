module Main where

-- import MyLib qualified (someFunc)

import NaiveParser (Error)
import System.Console.Haskeline
  ( InputT
  , defaultSettings
  , getInputLine
  , outputStrLn
  , runInputT
  )

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
      Just input -> do
        outputStrLn $ "Input was: " ++ input
        loop

-- MyLib.someFunc

-- yaq>
-- q)a:42
-- q)a :42
-- q)a: 42
-- q)a : 42
-- q)a
-- q)c
-- c is not defined