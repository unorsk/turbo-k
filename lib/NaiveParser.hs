module NaiveParser (Error (..), Parser (..)) where

data Error i e
  = EndOfInput -- Expected more input, but there is nothing
  | Unexpected i -- We didn't expect to find this element
  | CustomError e -- Extra errors the user may want to create
  | Empty -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \case
  [] -> Left [EndOfInput]
  hd : rest
    | predicate hd -> Right (hd, rest)
    | otherwise -> Left [Unexpected hd]

char :: Eq i => i -> Parser i e i
char i = satisfy (== i)

instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) -> Right (f output, rest)

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input ->
    case f input of
      Left err -> Left err
      Right (f', rest) ->
        case p rest of
          Left err -> Left err
          Right (output, rest') -> Right (f' output, rest')

newtype Parser i e a = Parser
  { runParser :: [i] -> Either [Error i e] (a, [i])
  }