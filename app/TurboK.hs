{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
-- module TurboK ( parseK ) where
module TurboK where
import Text.Megaparsec (Parsec, single, sepBy1, between, manyTill, sepBy, choice, MonadParsec (eof), empty, some, many, (<|>), try)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Char (letterChar, digitChar, hexDigitChar, string, char, spaceChar)
import Text.Megaparsec.Char.Lexer (charLiteral)

type ParserK = Parsec Void Text

data KExprs = KExprs [KExpr] deriving (Show, Eq)
data KExpr = KExprNoun KNoun KVerb KExpr | KExprTerm KTerm KExpr | KExprEmpty deriving (Show, Eq)
data KTerm = KTermNoun KNoun | KTermVerb KVerb deriving (Show, Eq)
data KVerb = KVerbWithAdverb KTerm KAdverb | KVerb KVerbTerm deriving (Show, Eq)
data KNoun = KNounTermExprs KTerm KExprs | KNounExprs0 KExprs | KNounExprs1 KExprs | KNoun KNounTerm deriving (Show, Eq)
data KVerbTerm = KVerbTerm KVerbBuiltIn | KVerbWithGets KVerbBuiltIn deriving (Show, Eq)
data KVerbBuiltIn = KVBIGet | KVBIPlus | KVBIMinus | KVBITimes | KVBIDropFloor deriving (Show, Eq)
data KNounTerm = KNounName KName | KNounInts KInts | KNounFloats KFloats | KNounString String | KNounSymbols KSymbols  deriving (Show, Eq)
data KName = KName String deriving (Show, Eq)
data KAdverb = AdverbEach | AdverbOverJoin | AdverbScanSplit | AdverbEachPrior | AdverbEachRight | AdverbEchLeft deriving (Show, Eq)
data KInts = KInts [Int]  deriving (Show, Eq)
data KFloats = KFloats [Float] deriving (Show, Eq)
data KString = KStringChars String | KStringBytes deriving (Show, Eq)
data Bytes = Bytes Bytes KHex KHex | EmptyBytes deriving (Show, Eq)
data KSymbols = KSymbols [KSymbol] deriving (Show, Eq)
data KSymbol = KSymbol String deriving (Show, Eq)
data KHex = KHex String deriving (Show, Eq)

parseK :: ParserK KExprs
parseK = parseExprs <* choice [empty, eof]

parseExprs :: ParserK KExprs
parseExprs = KExprs <$> sepBy parseKExpr (char ';')

parseKExpr :: ParserK KExpr
parseKExpr = parseKExprNoun <|> parseKExprTerm <|> parseKEmpty
  where
    parseKEmpty = do
      _ <- some spaceChar
      return KExprEmpty
    parseKExprTerm = do
      term <- parseKTerm
      expr <- parseKExpr
      return $ KExprTerm term expr
    parseKExprNoun = do
      noun <- parseKNoun
      verb <- parseKVerb
      expr <- parseKExpr
      return $ KExprNoun noun verb expr

parseKTerm :: ParserK KTerm
parseKTerm = KTermNoun <$> parseKNoun <|> KTermVerb <$> parseKVerb

parseKVerb :: ParserK KVerb
parseKVerb = do
  parseKVerWithAdverb
  <|> KVerb <$> parseKVerbTerm
    where
      parseKVerWithAdverb = do
        term <- parseKTerm
        KVerbWithAdverb term <$> parseKAdverb

parseKNoun :: ParserK KNoun
parseKNoun =
  try parseKNounTermExprs
  <|> try parseKNounExprs0
  <|> try parseKNounExprs1
  <|> try parseKNounNoun
  where
    parseKNounTermExprs = do
      term <- parseKTerm
      exprs <- between (char '[') (char ']') parseExprs
      return $ KNounTermExprs term exprs
    parseKNounExprs0 = KNounExprs0 <$> between (char '(') (char ')') parseExprs
    parseKNounExprs1 = KNounExprs0 <$> between (char '{') (char '}') parseExprs
    parseKNounNoun = KNoun <$> parseKNounTerm


parseKNounTerm :: ParserK KNounTerm
parseKNounTerm = do
  KNounName <$> parseKName
  <|> KNounInts <$> parseKInts
  <|> KNounFloats <$> parseKFloats
  <|> KNounString <$> parseKString
  <|> KNounSymbols <$> parseKSymbols

parseKString :: ParserK String
parseKString = char '"' >> manyTill charLiteral (char '"')

parseKFloats :: ParserK KFloats
parseKFloats = KFloats <$> sepBy1 parseKFloat (char ' ')
  where
    parseKFloat = do
      h <- some digitChar
      _ <- char '.'
      t <- some digitChar
      return (read (h <> "." <> t) :: Float)

parseKInts :: ParserK KInts
parseKInts = KInts <$> sepBy1 parseKInt (char ' ')
  where
    parseKInt = do
      int <- some digitChar
      return (read int :: Int)

parseKSymbols :: ParserK KSymbols
parseKSymbols = KSymbols <$> some parseKSymbol

parseKSymbol :: ParserK KSymbol
parseKSymbol = do
  _ <- single '`'
  KSymbol <$> (many letterChar <|> (many letterChar <> many digitChar))

parseKHex :: ParserK KHex
parseKHex = KHex <$> many hexDigitChar

parseKVerbTerm :: ParserK KVerbTerm
parseKVerbTerm = KVerbTerm <$> parseKVerbBuiltIn <|> (KVerbWithGets <$> parseKVerbBuiltIn <* char ':')

parseKVerbBuiltIn :: ParserK KVerbBuiltIn
parseKVerbBuiltIn = do
  KVBIGet <$ char ':'
  <|> KVBIPlus <$ char '+'
  <|> KVBIMinus <$ char '-'
  <|> KVBITimes <$ char '*'
  <|> KVBIDropFloor <$ char '_'

parseKAdverb :: ParserK KAdverb
parseKAdverb = do
  AdverbEach <$ string "'"
  <|> AdverbOverJoin <$ string "/"
  <|> AdverbScanSplit <$ string "\\"
  <|> AdverbEachPrior <$ string "':"
  <|> AdverbEachRight <$ string "/:"
  <|> AdverbEchLeft <$ string "\\:"

-- TODO handle dots in names
parseKName :: ParserK KName
parseKName = KName <$> (some letterChar <|> (some letterChar <> many digitChar))
