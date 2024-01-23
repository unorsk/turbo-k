{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE OverloadedStrings #-}
-- module TurboK ( parseK ) where
module TurboK where
import Text.Megaparsec (Parsec, single, sepBy1, between, manyTill, sepBy, choice, MonadParsec (eof), empty, some, many, (<|>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Char (letterChar, digitChar, hexDigitChar, string, char, spaceChar)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Text.Megaparsec.Debug (dbg)
import Control.Applicative (Alternative, (<**>))

type ParserK = Parsec Void Text

data KExprs = KExprs [KExpr] deriving (Show, Eq)
data KExpr =  KExprNoun KNoun KVerb KExpr | KExprTerm KTerm KExpr | KExprEmpty deriving (Show, Eq)
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

chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan where
  scan = p <**> rst
  rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id
  
parseK :: ParserK KExprs
parseK = dbg "parseExprs" exprs <* choice [empty, eof]

exprs :: ParserK KExprs
exprs = KExprs <$> sepBy kexpr (char ';')

kexpr :: ParserK KExpr
kexpr = dbg "parseKExprNoun" parseKExprNoun <|> parseKExprTerm <|> parseKEmpty
  where
    parseKEmpty = do
      _ <- some spaceChar
      return KExprEmpty
    parseKExprTerm = do
      term <- kTerm
      expr <- kexpr
      return $ KExprTerm term expr
    parseKExprNoun = do
      noun <- kNoun
      verb <- kVerb
      expr <- kexpr
      return $ KExprNoun noun verb expr

kTerm :: ParserK KTerm
kTerm = KTermNoun <$> kNoun <|> KTermVerb <$> kVerb

kVerb :: ParserK KVerb
kVerb = do
  parseKVerWithAdverb
  <|> KVerb <$> kVerbTerm
    where
      parseKVerWithAdverb = do
        term <- kTerm
        KVerbWithAdverb term <$> kAdverb

kNoun :: ParserK KNoun
kNoun =
  parseKNounTermExprs
  <|> parseKNounExprs0
  <|> parseKNounExprs1
  <|> parseKNounNoun
  where
    parseKNounTermExprs = do
      term <- kTerm
      exprs1 <- between (char '[') (char ']') exprs
      return $ KNounTermExprs term exprs1
    parseKNounExprs0 = KNounExprs0 <$> between (char '(') (char ')') exprs
    parseKNounExprs1 = KNounExprs1 <$> between (char '{') (char '}') exprs
    parseKNounNoun = KNoun <$> kNounTerm

kNounTerm :: ParserK KNounTerm
kNounTerm = do
  KNounName <$> kName
  <|> KNounInts <$> kInts
  <|> KNounFloats <$> kFloats
  <|> KNounString <$> kString
  <|> KNounSymbols <$> kSymbols

kString :: ParserK String
kString = char '"' >> manyTill charLiteral (char '"')

kFloats :: ParserK KFloats
kFloats = KFloats <$> sepBy1 parseKFloat (char ' ')
  where
    parseKFloat = do
      h <- some digitChar
      _ <- char '.'
      t <- some digitChar
      return (read (h <> "." <> t) :: Float)

kInts :: ParserK KInts
kInts = KInts <$> sepBy1 parseKInt (char ' ')
  where
    parseKInt = do
      int <- some digitChar
      return (read int :: Int)

kSymbols :: ParserK KSymbols
kSymbols = KSymbols <$> some kSymbol

kSymbol :: ParserK KSymbol
kSymbol = do
  _ <- single '`'
  KSymbol <$> (many letterChar <|> (many letterChar <> many digitChar))

parseKHex :: ParserK KHex
parseKHex = KHex <$> many hexDigitChar

kVerbTerm :: ParserK KVerbTerm
kVerbTerm = KVerbTerm <$> kVerbBuiltIn <|> (KVerbWithGets <$> kVerbBuiltIn <* char ':')

kVerbBuiltIn :: ParserK KVerbBuiltIn
kVerbBuiltIn = do
  KVBIGet <$ char ':'
  <|> KVBIPlus <$ char '+'
  <|> KVBIMinus <$ char '-'
  <|> KVBITimes <$ char '*'
  <|> KVBIDropFloor <$ char '_'

kAdverb :: ParserK KAdverb
kAdverb = do
  AdverbEach <$ string "'"
  <|> AdverbOverJoin <$ string "/"
  <|> AdverbScanSplit <$ string "\\"
  <|> AdverbEachPrior <$ string "':"
  <|> AdverbEachRight <$ string "/:"
  <|> AdverbEchLeft <$ string "\\:"

-- TODO handle dots in names
kName :: ParserK KName
kName = KName <$> (some letterChar <|> (some letterChar <> many digitChar))
