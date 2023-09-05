module Grammar where

data Expr
  = E1 Noun Verb Expr
  | E2 Term Expr
  | Empty

data Noun
data Verb = V1 Term ADVERB | V2 VERB

type Term = Either Noun Verb

-- Lexer

data ADVERB
data VERB

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