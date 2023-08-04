module Parserq (Parserq, parseq, evalq, parseTerm, parseFloat, parseOperation, parseList) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data Result
  = RInt Integer
  | RFloat Float
  | RBoolean Boolean
  | RList [Result]
  | RError String
  deriving (Eq)

instance Show Result where
  show (RInt i) = show i
  show (RFloat f) = show f
  show (RBoolean b) = show b
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
  deriving (Eq, Show)

data Term
  = MyInt Integer
  | MyFloat Float
  | Boolean Boolean
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
parseExpr' = Expr <$> (parseTerm <* many spaceChar) <*> parseOperation <* many spaceChar <*> parseTerm

parseExpr :: Parserq Expr
parseExpr =
  try parseList
    <|> try parseExpr'
    <|> Term <$> try parseTerm

parseList :: Parserq Expr
parseList = List <$> between (single '(') (single ')') (sepBy parseTerm (many spaceChar >> single ';' >> many spaceChar))

parseOperation :: Parserq Operation
parseOperation =
  choice
    [ Add <$ single '+',
      Subtract <$ single '-'
    ]

parseBoolean :: Parserq Boolean
parseBoolean = do
  c <- oneOf ['0', '1']
  _ <- single 'b'
  return (if c == '1' then B1 else B0)

-- parseNumber :: Parserq Number
-- parseNumber = MyFloat <$> try parseFloat <|> MyInt <$> try parseInt

parseTerm :: Parserq Term
parseTerm = Boolean <$> try parseBoolean <|> MyFloat <$> try parseFloat <|> MyInt <$> try parseInt

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

opeartion :: (Num a0) => Operation -> (a0 -> a0 -> a0)
opeartion = \case
  Add -> (+)
  Subtract -> (-)

evalq :: Expr -> Result
evalq = \case
  Expr (MyInt i1) o (MyInt i2) -> RInt $ opeartion o i1 i2
  Expr (MyFloat f1) o (MyFloat f2) -> RFloat $ opeartion o f1 f2
  Expr (MyInt i1) o (MyFloat f2) -> RFloat $ opeartion o (fromIntegral i1) f2
  Expr (MyFloat f1) o (MyInt i2) -> RFloat $ opeartion o f1 (fromIntegral i2)
  Expr (MyInt _) _ (Boolean _) -> RError "Cannot mix up an int and a boolean"
  Expr (Boolean _) _ (MyInt _) -> RError "Cannot mix up an int and a boolean"
  Expr (MyFloat _) _ (Boolean _) -> RError "Cannot mix up a float and a boolean"
  Expr (Boolean _) _ (MyFloat _) -> RError "Cannot mix up a float and a boolean"
  Term t -> evalTerm t
  List l -> RList $ map evalTerm l
  Expr (Boolean _) _ (Boolean _) -> RError "Cannot add a boolean to a boolean"

evalTerm :: Term -> Result
evalTerm = \case
  MyInt i -> RInt i
  MyFloat f -> RFloat f
  Boolean b -> RBoolean b

parseq :: Parserq Expr
parseq = parseExpr <* eof

-- parseq = parseNumber <* eof