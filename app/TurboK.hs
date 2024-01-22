{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
module TurboK (parseKExpr, parseK) where
import Text.Megaparsec (Parsec, single, sepBy1, between, manyTill, sepBy, choice, MonadParsec (eof, try), empty, some, many, (<|>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Char (letterChar, digitChar, hexDigitChar, string, char, spaceChar)
import Text.Megaparsec.Char.Lexer (charLiteral)

type ParserK = Parsec Void Text

data KExprs = KExprs [KExpr] deriving (Show)
data KExpr = KExprNoun KNoun KVerb KExpr | KExprTerm KTerm KExpr | KExprEmpty deriving (Show)
-- data KExpr = KExprNoun KNoun KVerb  | KExprTerm KTerm  | KExprEmpty deriving (Show)
data KTerm = KTermNoun KNoun | KTermVerb KVerb deriving (Show)
-- data KVerb = KVerbWithAdverb KTerm KAdverb | KVerb KVerbTerm deriving (Show)
-- data KNoun = KNounTermExprs KTerm KExprs | KNounExprs0 KExprs | KNounExprs1 KExprs | KNoun KNounTerm deriving Show
data KVerb = KVerbWithAdverb  KAdverb | KVerb KVerbTerm deriving (Show)
data KNoun = KNounTermExprs  KExprs | KNounExprs0 KExprs | KNounExprs1 KExprs | KNoun KNounTerm deriving Show
data KVerbTerm = KVerbTerm KVerbBuiltIn | KVerbWithGets KVerbBuiltIn deriving Show
data KVerbBuiltIn = KVBIGet | KVBIPlus | KVBIMinus | KVBITimes | KVBIDropFloor deriving Show
data KNounTerm = KNounName KName | KNounInts KInts | KNounFloats KFloats | KNounString String | KNounSymbols KSymbols  deriving (Show)
data KName = KName String deriving (Show)
data KAdverb = AdverbEach | AdverbOverJoin | AdverbScanSplit | AdverbEachPrior | AdverbEachRight | AdverbEchLeft deriving (Show)
data KInts = KInts [Int]  deriving (Show)
data KFloats = KFloats [Float] deriving (Show)
data KString = KStringChars String | KStringBytes deriving (Show)
data Bytes = Bytes Bytes KHex KHex | EmptyBytes deriving (Show)
data KSymbols = KSymbols [KSymbol] | KSingleSymbol KSymbol deriving (Show)
data KSymbol = KSymbol String deriving (Show)
data KHex = KHex String deriving Show

parseK :: ParserK KExprs
parseK = parseExprs <* choice [empty, eof]

parseExprs :: ParserK KExprs
parseExprs = KExprs <$> sepBy parseKExpr (char ';')

-- data KExpr = KExprNoun KNoun KVerb KExpr | KExprTerm KTerm KExpr | KExprEmpty deriving (Show)
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
parseKTerm = do
  KTermNoun <$> parseKNoun
  <|> KTermVerb <$> parseKVerb

parseKVerb :: ParserK KVerb
parseKVerb = do
  parseKVerWithAdverb
  <|> KVerb <$> parseKVerbTerm
    where
      parseKVerWithAdverb = do
        -- term <- parseKTerm
        -- KVerbWithAdverb term <$> parseKAdverb
        KVerbWithAdverb  <$> parseKAdverb

parseKNoun :: ParserK KNoun
parseKNoun =
  parseKNounTermExprs
  <|> parseKNounExprs0
  <|> parseKNounExprs1
  <|> parseKNounNoun
  where
    parseKNounTermExprs = do
      -- term <- parseKTerm
      -- exprs <- between (char '[') (char ']') parseExprs
      -- return $ KNounTermExprs term exprs
      exprs <- between (char '[') (char ']') parseExprs
      return $ KNounTermExprs exprs
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
parseKFloats = do
  KFloats <$> sepBy1 parseKFloat (char ' ')
  where
    parseKFloat = do
      h <- some digitChar
      _ <- char '.'
      t <- some digitChar
      return (read (h <> "." <> t) :: Float)

parseKInts :: ParserK KInts
parseKInts = do
  KInts <$> sepBy1 parseKInt (char ' ')
  where
    parseKInt = do
      int <- some digitChar
      return (read int :: Int)

parseKSymbols :: ParserK KSymbols
parseKSymbols = do
  smbls <- some parseKSymbol
  return $ if length smbls > 1 then KSymbols smbls else KSingleSymbol $ head smbls

parseKSymbol :: ParserK KSymbol
parseKSymbol = do
  _ <- single '`'
  KSymbol <$> (many letterChar <|> (many letterChar <> many digitChar))

parseKHex :: ParserK KHex
parseKHex = KHex <$> many hexDigitChar

parseKVerbTerm :: ParserK KVerbTerm
parseKVerbTerm = do
  KVerbTerm <$> parseKVerbBuiltIn
  <|> (KVerbWithGets <$> parseKVerbBuiltIn <* char ':')

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
