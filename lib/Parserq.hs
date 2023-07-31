module Parserq (Parserq, parseq, parseNumber, parseFloat, parseOperation, parseList) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data Expr
  = Expr Number Operation Number
  | Number Number
  -- | Expr Expr Operation Expr
  deriving (Eq, Show)

data List
  = List [Number]
  deriving (Eq, Show)

data Operation
  = Add
  | Subtract
  -- | Multiply
  -- | Divide
  deriving (Eq, Show)

data Number
  = MyInt Integer
  | MyFloat Float
  deriving (Eq, Show)

type Parserq = Parsec Void Text

parseExpr' :: Parserq Expr
parseExpr' = Expr <$> (parseNumber <* many spaceChar) <*> parseOperation <* many spaceChar <*> parseNumber

parseExpr :: Parserq Expr
parseExpr = try parseExpr' <|>
  Number <$> try parseNumber

parseList :: Parserq List
parseList = List <$> between (single '(')  (single ')') (sepBy parseNumber (many spaceChar >> single ';' >> many spaceChar ))

parseOperation :: Parserq Operation
parseOperation =
  choice
    [ Add <$ single '+'
    , Subtract <$ single '-'
    ]

parseNumber :: Parserq Number
parseNumber = MyFloat <$> try parseFloat <|> MyInt <$> try parseInt

parseInt :: Parserq Integer
parseInt = do
  h <- many digitChar
  return (read h :: Integer)

parseFloat :: Parserq Float
parseFloat = do
  h <- many digitChar
  _ <- char '.'
  t <- many digitChar
  return (read (h <> "." <> t) :: Float)

parseq :: Parserq Expr
parseq = parseExpr <* eof
-- parseq = parseNumber <* eof