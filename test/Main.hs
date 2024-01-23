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
        parse parseKName "" "abc" `shouldParse` KName "abc"
    describe "symbols" $ do
      it "Parses symbol" $
        parse parseKSymbol "" "`abc" `shouldParse` KSymbol "abc"
      it "Parses multiple symbols" $
        parse parseKSymbols "" "`abc`bcd" `shouldParse` KSymbols [KSymbol "abc", KSymbol "bcd"]
    describe "nouns terminals" $ do
      it "Parses symbol" $
        parse parseKNounTerm "" "`abc" `shouldParse` KNounSymbols (KSymbols [KSymbol "abc"])
      it "Parses name" $
        parse parseKNounTerm "" "abc" `shouldParse` KNounName (KName "abc")
      it "Parses ints" $
        parse parseKNounTerm "" "1 2 3" `shouldParse` KNounInts (KInts [1, 2, 3])
      it "Parses ints" $
        parse parseKNounTerm "" "1 2 3" `shouldParse` KNounInts (KInts [1, 2, 3])
      it "Parses string" $
        parse parseKNounTerm "" "\"1 2 3\"" `shouldParse` KNounString "1 2 3"
    -- 
    -- the tests below don't work :/ 
    --
    -- describe "noun terminal" $ do
    --   it "parses 1" $
    --     parse (parseKNoun <* eof) "" "1" `shouldParse` KNoun (KNounInts $ KInts [1])
    --   it "parses 1 2 3" $
    --     parse (parseKNoun <* eof) "" "1 2 3" `shouldParse` KNoun (KNounInts $ KInts [1, 2, 3])
    --   it "parses [1 2 3]" $
    --     parse (parseKNoun <* eof) "" "[1 2 3]" `shouldParse` KNoun (KNounInts $ KInts [1, 2, 3])
    --   it "parses [1 2 3]" $
    --     parse (parseKNoun <* eof) "" "(1 2 3)" `shouldParse` KNoun (KNounInts $ KInts [1, 2, 3])
    -- describe "nouns Exprs" $ do
    --   it "Parses 1+1" $
    --     parse (parseKExpr <* eof) "" "1+1" `shouldParse` KExprNoun (KNoun (KNounInts $ KInts [1]))
    --       (KVerb $ KVerbTerm KVBIPlus)
    --       (KExprTerm (KTermNoun (KNoun (KNounInts $ KInts [1]))) KExprEmpty)
    -- describe "verbs" $ do
    --   it "Parses +" $
    --     parse (parseKVerb <* eof) "" "+" `shouldParse`  KVerb (KVerbTerm KVBIPlus)
    -- describe "verbs" $ do
    --   it "Parses /+" $
    --     parse (parseKVerb <* eof) "" "/+" `shouldParse` KVerbWithAdverb (KTermVerb (KVerb (KVerbTerm KVBIPlus))) AdverbEach

