module Grammarq
  ( QAst (..)
  , QExpr (..)
  , QTerm (..)
  , QVerb (..)
  , QAdverb (..)
  , QTermVerb (..)
  , QNoun (..)
  ) where

data QAst
  = QAst [QExpr]
  | QExpr

data QExpr
  = QExpr1 QNoun QVerb QExpr
  | QExpr2 QTerm QExpr
  | Empty

type QTerm = Either QNoun QVerb
data QVerb = V1 QTerm QAdverb | V2 QTermVerb

data QNoun
  = QNounBooleans [Bool]
  | QNounInts [Integer]

data QAdverb
data QTermVerb
  = QTermVerbPlus
  | QTermVerbMinus

{-

<Exprs>  ::=  <Exprs> ";" <expr>  |  <expr>
<expr>   ::=  <noun> <verb> <expr>  |  <term> <expr>  |  empty
<term>   ::=  <noun>  |  <verb>
<verb>   ::=  <term> <Adverb>  |  <Verb>
<noun>   ::=  <term> "[" <Exprs> "]"  |  "(" <Exprs> ")"  |  "{" <Exprs> "}"  |  <Noun>

Further, <Adverb> <Verb> <Noun> can be defined,
with minor variations between dialects, as:

<Adverb> ::=  "'" | "/" | "\" | "':" | "/:" | "\:"
<Verb>   ::=  <Verb1> | <Verb1> ":"
<Verb1>  ::=  ":" | "+" | "-" | "*" | "%" | "!" | "&" | "|" | "<" | ">" | "=" | "~" | "," |
              "^" | "#" | "_" | "$" | "?" | "@" | "." | <Digit> ":"
<Noun>   ::=  <Names>  |  <Ints>  |  <Floats>  |  <String>  |  <Symbols>
<Names>  ::=  <Names> "." <Name>  |  <Name>
<Name>   ::=  <Letter>  |  <Name> <Letter>  |  <Name> <Digit>
<Ints>   ::=  <Int>  |  <Ints> " " <Int>
<Int>    ::=  "-" <Digits>  |  <Digits>
<Floats> ::=  <Float>  |  <Floats> " " <Float>
<Float>  ::=  <Int>  |  <Int> "." <Digits>  |  <Int> "." <Digits> "e" <Int>
<String> ::=  '"' <Chars> '"'  |  "0x" <Bytes>
<Chars>  ::=  <Chars> <Char>  |  empty
<Char>   ::=  "\0" | "\t" | "\n" | "\r" | '\"' | "\\" | any
<Bytes>  ::=  <Bytes> <Hex> <Hex>  |  empty
<Symbols>::=  <Symbols> <Symbol>  |  <Symbol>
<Symbol> ::=  "`"  |  "`" <Name>  |  "`" <String>
<Digits> ::=  <Digit>  |  <Digits> <Digit>
<Digit>  ::=  "0" | .. | "9"
<Hex>    ::=  <Digit> | "a" | .. | "f"
<Letter> ::=  "A" | .. | "Z" | "a" | .. | "z"

-}