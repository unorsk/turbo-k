{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module TurboK (Exp(..)) where 
import Text.Megaparsec (Parsec)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec.Char (letterChar, digitChar)
import Control.Applicative (many, (<|>), (<*>), (<**>))

type ParserK = Parsec Void Text

data KName = KName String deriving (Show)

-- data TNoun = TNoun
-- data TVerb = TVerb
-- data Term = Noun TNoun | Verb TVerb
-- data Exp = NVEExpr TNoun TVerb Exp | TExp Term Exp

data Exprs = Exprs Exprs Expr | SingleExpr Expr deriving (Show)
data Expr = NounExpr Noun Verb Expr | TermExpr Term Expr | Empty deriving (Show)
data Term = TermNoun Noun | TermVerb Verb deriving (Show)
data Verb = TermAdverb Term Adverb | SingleVerb Verb1 deriving (Show)
data KNoun = KNames [KName] deriving (Show)
data Adverb = Adverb Char deriving (Show)
data Verb1 = Verb1 Char | Verb1Colon Char deriving (Show)
-- data Noun1 = NounNames Names | NounInts Ints | NounFloats Floats | NounString String | NounSymbols Symbols deriving (Show)
-- data Names = Names Names Name | SingleName Name deriving (Show)
-- data Name = NameChar Char | NameName Name Char | NameDigit Name Char deriving (Show)
data Ints = Ints Ints Int | SingleInt Int deriving (Show)
data Floats = Floats Floats Float | SingleFloat Float deriving (Show)
data KString = KStringChars String | KStringBytes deriving (Show)
-- data Chars = Chars Chars Char | EmptyChars deriving (Show)
data Bytes = Bytes Bytes Hex Hex | EmptyBytes deriving (Show)
data KSymbols = KSymbols KSymbols KSymbol | KSingleSymbol KSymbol deriving (Show)
data KSymbol = KSymbol KName deriving (Show)
-- data Hex = HexChar Char deriving (Show)

parseName :: ParserK KName
parseName = KName <$> (many letterChar <|> (many letterChar <> many digitChar))

data Exp = Exp String