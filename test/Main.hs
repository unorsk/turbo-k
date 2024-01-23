module Main (main) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import TurboK

main :: IO ()
main = hspec $ do
  -- testEval
  testParse

testParse :: SpecWith ()
testParse =
  describe "parse" $ do
    describe "names" $ do
      it "Parses name" $
        parse kName "" "abc" `shouldParse` KName "abc"
    describe "symbols" $ do
      it "Parses symbol" $
        parse kSymbol "" "`abc" `shouldParse` KSymbol "abc"
      it "Parses multiple symbols" $
        parse kSymbols "" "`abc`bcd" `shouldParse` KSymbols [KSymbol "abc", KSymbol "bcd"]
    describe "nouns terminals" $ do
      it "Parses symbol" $
        parse kNounTerm "" "`abc" `shouldParse` KNounSymbols (KSymbols [KSymbol "abc"])
      it "Parses name" $
        parse kNounTerm "" "abc" `shouldParse` KNounName (KName "abc")
      it "Parses ints" $
        parse kNounTerm "" "1 2 3" `shouldParse` KNounInts (KInts [1, 2, 3])
      it "Parses ints" $
        parse kNounTerm "" "1 2 3" `shouldParse` KNounInts (KInts [1, 2, 3])
      it "Parses string" $
        parse kNounTerm "" "\"1 2 3\"" `shouldParse` KNounString "1 2 3"
    -- 
    -- the tests below don't work :/ 
    --
    -- describe "noun terminal" $ do
    --   it "parses 1" $
    --     parse (kNoun <* eof) "" "1" `shouldParse` KNoun (KNounInts $ KInts [1])
    --   it "parses 1 2 3" $
    --     parse (kNoun <* eof) "" "1 2 3" `shouldParse` KNoun (KNounInts $ KInts [1, 2, 3])
    --   it "parses [1 2 3]" $
    --     parse (kNoun <* eof) "" "[1 2 3]" `shouldParse` KNoun (KNounInts $ KInts [1, 2, 3])
    --   it "parses [1 2 3]" $
    --     parse (kNoun <* eof) "" "(1 2 3)" `shouldParse` KNoun (KNounInts $ KInts [1, 2, 3])
    -- describe "nouns Exprs" $ do
    --   it "Parses 1+1" $
    --     parse (kexpr <* eof) "" "1+1" `shouldParse` KExprNoun (KNoun (KNounInts $ KInts [1]))
    --       (KVerb $ KVerbTerm KVBIPlus)
    --       (KExprTerm (KTermNoun (KNoun (KNounInts $ KInts [1]))) KExprEmpty)
    -- describe "verbs" $ do
    --   it "Parses +" $
    --     parse (kVerb <* eof) "" "+" `shouldParse`  KVerb (KVerbTerm KVBIPlus)
    -- describe "verbs" $ do
    --   it "Parses /+" $
    --     parse (kVerb <* eof) "" "/+" `shouldParse` KVerbWithAdverb (KTermVerb (KVerb (KVerbTerm KVBIPlus))) AdverbEach

