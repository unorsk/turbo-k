module Parserq (Parserq, parseq, evalq) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Result
  = RInt Integer
  | RFloat Float
  | RBoolean Boolean
  | RList [Result]
  | RError String
  deriving (Eq)

instance Show Result where
  show (RInt i) = show i
  show (RFloat f) = show f <> "f"
  show (RBoolean b) = show b -- TODO
  show (RList l) = show l
  show (RError e) = e

data Expr
  = Expr Term Operation Term
  | Term Term
  | List [Term]
  deriving (Eq)

instance Show Expr where
  show (Expr t1 op t2) = show t1 <> " " <> show op <> " " <> show t2
  show (Term t) = show t
  show (List ts) = "(" <> show ts <> ")"

data Operation
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Eq, Show)

data Term
  = QInt Integer
  | QFloat Float
  | QDate Integer
  | QBoolean Boolean
  deriving (Eq, Show)

data Boolean
  = B1
  | B0
  deriving (Eq, Show)

-- data Number
--   = MyInt Integer
--   | MyFloat Float
--   deriving (Eq, Show)

type Parserq = Parsec Void Text

parseExpr' :: Parserq Expr
parseExpr' =
  Expr
    <$> (parseTerm <* many spaceChar)
    <*> parseOperation
    <* many spaceChar
    <*> parseTerm

parseExpr :: Parserq Expr
parseExpr =
  try parseList
    <|> try parseExpr'
    <|> Term
    <$> try parseTerm

-- <$> try comment

parseList :: Parserq Expr
parseList =
  List
    <$> between
      (single '(')
      (single ')')
      (sepBy parseTerm (many spaceChar >> single ';' >> many spaceChar))

parseOperation :: Parserq Operation
parseOperation =
  choice
    [ Add <$ single '+'
    , Subtract <$ single '-'
    , Multiply <$ single '*'
    , Divide <$ single '%'
    ]

parseDate :: Parserq Integer
parseDate = do
  y <- count 4 digitChar
  _ <- single '.'
  m <- count 2 digitChar
  _ <- single '.'
  d <- count 2 digitChar
  -- TODO (:
  let years = (read y :: Integer) - 2000
      months = (read m :: Integer) - 1
      days = read d :: Integer
   in return (years * 365 + months * 30 + days)

parseBoolean :: Parserq Boolean
parseBoolean = do
  c <- oneOf ['0', '1']
  _ <- single 'b'
  return (if c == '1' then B1 else B0)

-- parseNumber :: Parserq Number
-- parseNumber = MyFloat <$> try parseFloat <|> MyInt <$> try parseInt

comment :: Parserq ()
comment =
  L.space
    space1
    (L.skipLineComment "/")
    empty

parseTerm :: Parserq Term
parseTerm =
  QBoolean
    <$> try parseBoolean
      <|> QDate
    <$> try parseDate
      <|> QFloat
    <$> try parseFloat
      <|> QInt
    <$> try parseInt

parseInt :: Parserq Integer
parseInt = do
  h <- some digitChar
  return (read h :: Integer)

parseFloat :: Parserq Float
parseFloat = do
  h <- some digitChar
  _ <- char '.'
  t <- some digitChar
  return (read (h <> "." <> t) :: Float)

opeartion :: Operation -> (Integer -> Integer -> Integer)
opeartion o a0 a1 = case o of
  Add -> a0 + a1
  Subtract -> a0 - a1
  Multiply -> a0 * a1
  Divide -> fromInteger (round (fromIntegral a0 / fromIntegral a1))

opeartionF :: Fractional a0 => Operation -> (a0 -> a0 -> a0)
opeartionF = \case
  Add -> (+)
  Subtract -> (-)
  Multiply -> (*)
  Divide -> (/)

evalq :: Expr -> Result
evalq = \case
  Expr (QDate i1) o (QInt i2) -> RInt $ opeartion o i1 i2
  Expr (QInt i1) o (QDate i2) -> RInt $ opeartion o i1 i2
  Expr (QDate i1) o (QDate i2) -> RInt $ opeartion o i1 i2
  Expr (QInt i1) o (QInt i2) -> RInt $ opeartion o i1 i2
  Expr (QFloat f1) o (QFloat f2) -> RFloat $ opeartionF o f1 f2
  Expr (QInt i1) o (QFloat f2) -> RFloat $ opeartionF o (fromIntegral i1) f2
  Expr (QFloat f1) o (QInt i2) -> RFloat $ opeartionF o f1 (fromIntegral i2)
  Expr (QDate _) _ (QBoolean _) -> RError "Cannot mix up an int and a boolean"
  Expr (QBoolean _) _ (QDate _) -> RError "Cannot mix up an int and a boolean"
  Expr (QInt _) _ (QBoolean _) -> RError "Cannot mix up an int and a boolean"
  Expr (QBoolean _) _ (QInt _) -> RError "Cannot mix up an int and a boolean"
  Expr (QDate _) _ (QFloat _) -> RError "Cannot mix up a float and a boolean"
  Expr (QFloat _) _ (QDate _) -> RError "Cannot mix up a float and a boolean"
  Expr (QFloat _) _ (QBoolean _) -> RError "Cannot mix up a float and a boolean"
  Expr (QBoolean _) _ (QFloat _) -> RError "Cannot mix up a float and a boolean"
  Term t -> evalTerm t
  List l -> RList $ map evalTerm l
  Expr (QBoolean _) _ (QBoolean _) -> RError "Cannot add a boolean to a boolean"

evalTerm :: Term -> Result
evalTerm = \case
  QInt i -> RInt i
  QDate i -> RInt i
  QFloat f -> RFloat f
  QBoolean b -> RBoolean b

parseq :: Parserq Expr
parseq = parseExpr <* choice [empty, eof, comment]

-- parseq = parseNumber <* eof