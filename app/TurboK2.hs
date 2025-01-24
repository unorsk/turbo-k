module TurboK2 where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- Parser definitions
type Parser = Parsec Void String

data Expr
  = NumInt Int
  | -- | NumFloat Double
    Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Lambda [String] [Expr]
  | App Expr [Expr]
  deriving (Show, Eq)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parse numbers
number :: Parser Expr
-- number = oneOf [NumFloat <$> lexeme L.float, NumInt <$> lexeme L.decimal]
number = NumInt <$> lexeme L.decimal

-- Parse variables
variable :: Parser Expr
variable = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

-- Parse lambda expressions
lambda :: Parser Expr
lambda = do
  _ <- symbol "{"
  _ <- symbol "["
  params <- sepBy (lexeme ((:) <$> letterChar <*> many alphaNumChar)) (symbol ";")
  _ <- symbol "]"
  body <- sepBy expr (symbol ";")
  _ <- symbol "}"
  return $ Lambda params body

-- Parse expressions
term :: Parser Expr
term =
  choice
    [ number
    , variable
    , lambda
    , parens expr
    ]

binaryOp :: String -> (Expr -> Expr -> Expr) -> Parser (Expr -> Expr -> Expr)
binaryOp op f = f <$ symbol op

expr :: Parser Expr
expr =
  makeExprParser
    term
    [
      [ InfixR $ binaryOp "*" Mul
      , InfixR $ binaryOp "/" Div
      ]
    ,
      [ InfixR $ binaryOp "+" Add
      , InfixR $ binaryOp "-" Sub
      ]
    ]

-- Evaluator
type Env = [(String, Expr)]

eval :: Env -> Expr -> Either String Expr
eval _ (NumInt n) = Right $ NumInt n
eval env (Var x) = case lookup x env of
  Just v -> Right v
  Nothing -> Left $ "Undefined variable: " ++ x
eval env (Add e1 e2) = binOp (+) env e1 e2
eval env (Sub e1 e2) = binOp (-) env e1 e2
eval env (Mul e1 e2) = binOp (*) env e1 e2
eval env (Div e1 e2) = do
  v2 <- eval env e2
  case v2 of
    NumInt 0 -> Left "Division by zero"
-- _ -> binOp (/) env e1 e2
eval env (Lambda params body) = Right $ Lambda params body
eval env (App (Lambda params body) args) = do
  argVals <- mapM (eval env) args
  let env' = zip params argVals ++ env
  last <$> mapM (eval env') body
eval _ (App _ _) = Left "Invalid function application"

binOp
  :: (Int -> Int -> Int) -> Env -> Expr -> Expr -> Either String Expr
binOp op env e1 e2 = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (NumInt n1, NumInt n2) -> return $ NumInt (n1 `op` n2)
    _ -> Left "Type error: expected numbers"

-- Helper to parse and evaluate
parseAndEval :: String -> Either String Expr
parseAndEval input = case runParser (sc *> expr <* eof) "" input of
  Left err -> Left (errorBundlePretty err)
  Right ast -> eval [] ast
