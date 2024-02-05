{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE StrictData #-}
-- module TurboK ( parseK ) where
module TurboK where
import Text.Megaparsec (Parsec, single, sepBy1, between, manyTill, sepBy, MonadParsec (eof), empty, some, many, (<|>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Char (letterChar, digitChar, hexDigitChar, string, char, spaceChar)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Text.Megaparsec.Debug (dbg)
import Control.Applicative (Alternative, (<**>))

type KP = Parsec Void Text

data Exprs        = Exprs [Expr] deriving (Show, Eq)
data Expr         = ExprNoun Noun Verb Expr | ExprTerm Term Expr | ExprEmpty deriving (Show, Eq)
data Term         = TermNoun Noun | TermVerb Verb deriving (Show, Eq)
data Verb         = VerbWithAdverb Term Adverb | Verb VerbTerm deriving (Show, Eq)
data Noun         = NounExprs0 Term Exprs | NounExprs1 Exprs | NounExprs2 Exprs | Noun NounTerm deriving (Show, Eq)
data VerbTerm     = VerbTerm VerbBuiltIn | VerbWithGets VerbBuiltIn deriving (Show, Eq)
data VerbBuiltIn  = VBIGet | VBIPlus | VBIMinus | VBITimes | VBIDropFloor deriving (Show, Eq)
data NounTerm     = NounName Name | NounInts Ints | NounFloats Floats | NounString String | NounSymbols Symbols deriving (Show, Eq)
data Name         = Name String deriving (Show, Eq)
data Adverb       = AdverbEach | AdverbOverJoin | AdverbScanSplit | AdverbEachPrior | AdverbEachRight | AdverbEchLeft deriving (Show, Eq)
data Ints         = Ints [Int]  deriving (Show, Eq)
data Floats       = Floats [Float] deriving (Show, Eq)
data Str          = StrChars String | StrBytes deriving (Show, Eq)
data Bytes        = Bytes Bytes Hex Hex | EmptyBytes deriving (Show, Eq)
data Symbols      = Symbols [Symbol] deriving (Show, Eq)
data Symbol       = Symbol String deriving (Show, Eq)
data Hex          = Hex String deriving (Show, Eq)

chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan where
  scan = p <**> rst
  rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id

parseK :: KP Exprs
parseK = dbg "parseExprs" exprs <* (empty <|> eof)

exprs :: KP Exprs
exprs = Exprs <$> sepBy expr (char ';') -- this one

expr :: KP Expr
expr = dbg "parseKExprNoun" exprNoun <|> exprTerm <|> empty'
  where
    exprNoun = ExprNoun <$> noun <*> verb <*> expr
    exprTerm = ExprTerm <$> term  <*> expr
    empty' = return ExprEmpty <* some spaceChar

term :: KP Term
term = TermNoun <$> noun <|> TermVerb <$> verb

verb :: KP Verb
verb = do
  VerbWithAdverb <$> term <*> adverb
  <|> Verb <$> verbTerm

noun :: KP Noun
noun =
  Noun <$> nounTerm
  <|> NounExprs0 <$> term <*> between (char '[') (char ']') exprs
  <|> NounExprs1 <$> between (char '(') (char ')') exprs
  <|> NounExprs2 <$> between (char '{') (char '}') exprs

nounTerm :: KP NounTerm
nounTerm = do
  NounName <$> name
  <|> NounInts <$> ints
  <|> NounFloats <$> floats
  <|> NounString <$> kstr
  <|> NounSymbols <$> symbols

kstr :: KP String
kstr = char '"' >> manyTill charLiteral (char '"')

floats :: KP Floats
floats = Floats <$> sepBy1 parseKFloat (char ' ')
  where
    parseKFloat = do
      h <- some digitChar
      _ <- char '.'
      t <- some digitChar
      return (read (h <> "." <> t) :: Float)

ints :: KP Ints
ints = Ints <$> sepBy1 parseKInt (char ' ')
  where
    parseKInt = do
      int <- some digitChar
      return (read int :: Int)

symbols :: KP Symbols
symbols = Symbols <$> some kSymbol

kSymbol :: KP Symbol
kSymbol = Symbol <$ single '`' <*> (many letterChar <|> (many letterChar <> many digitChar))

hex :: KP Hex
hex = Hex <$> many hexDigitChar

verbTerm :: KP VerbTerm
verbTerm = VerbTerm <$> verbBuiltIn <|> (VerbWithGets <$> verbBuiltIn <* char ':')

verbBuiltIn :: KP VerbBuiltIn
verbBuiltIn = do
  VBIGet <$ char ':'
  <|> VBIPlus <$ char '+'
  <|> VBIMinus <$ char '-'
  <|> VBITimes <$ char '*'
  <|> VBIDropFloor <$ char '_'

adverb :: KP Adverb
adverb = do
  AdverbEach <$ string "'"
  <|> AdverbOverJoin <$ string "/"
  <|> AdverbScanSplit <$ string "\\"
  <|> AdverbEachPrior <$ string "':"
  <|> AdverbEachRight <$ string "/:"
  <|> AdverbEchLeft <$ string "\\:"

-- TODO handle dots in names
name :: KP Name
name = Name <$> (some letterChar <|> (some letterChar <> many digitChar))
