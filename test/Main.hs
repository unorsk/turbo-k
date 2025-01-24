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
  -- describe "parse" $ do
  --   describe "names" $ do
  --     it "Parses name" $
  --       parse name "" "abc" `shouldParse` Name "abc"
  --   describe "symbols" $ do
  --     it "Parses symbol" $
  --       parse kSymbol "" "`abc" `shouldParse` Symbol "abc"
  --     it "Parses multiple symbols" $
  --       parse symbols "" "`abc`bcd" `shouldParse` Symbols [Symbol "abc", Symbol "bcd"]
  --   describe "nouns terminals" $ do
  --     it "Parses symbol" $
  --       parse nounTerm "" "`abc" `shouldParse` NounSymbols (Symbols [Symbol "abc"])
  --     it "Parses name" $
  --       parse nounTerm "" "abc" `shouldParse` NounNames (Names [Name "abc"])
  --     it "Parses ints" $
  --       parse nounTerm "" "1 2 3" `shouldParse` NounInts (Ints [1, 2, 3])
  --     it "Parses string" $
  --       parse nounTerm "" "\"1 2 3\"" `shouldParse` NounString "1 2 3"
    -- 
    -- the tests below don't work :/ 
    --
    describe "noun terminal" $ do
      it "parses 1" $
        parse (noun <* eof) "" "1" `shouldParse` Noun (NounInts $ Ints [1])
      it "parses 1 2 3" $
        parse (noun <* eof) "" "1 2 3" `shouldParse` Noun (NounInts $ Ints [1, 2, 3])
        
      it "parses [1 2 3]" $
        parse (noun <* eof) "" "[1 2 3]" `shouldParse` Noun (NounInts $ Ints [1, 2, 3])

      -- cabal test --test-options="-m noun"

      -- it "parses (1 2 3)" $
      --   parse (noun <* eof) "" "(1 2 3)" `shouldParse` Noun (NounInts $ Ints [1, 2, 3])
      -- it "parses 1 2 3" $
      --   parse (noun <* eof) "" "1 2 3" `shouldParse` Noun (NounInts $ Ints [1, 2, 3])
    -- describe "nouns Exprs" $ do
    --   it "Parses 1+1" $
    --     parse (expr <* eof) "" "1+1" `shouldParse` ExprNoun (Noun (NounInts $ Ints [1]))
    --       (Verb $ VerbTerm VBIPlus)
    --       (ExprTerm (TermNoun (Noun (NounInts $ Ints [1]))) ExprEmpty)
    -- describe "verbs" $ do
    --   it "Parses +" $
    --     parse (verb <* eof) "" "+" `shouldParse`  Verb (VerbTerm VBIPlus)
    -- describe "verbs" $ do
    --   it "Parses /+" $
    --     parse (verb <* eof) "" "/+" `shouldParse` VerbWithAdverb (TermVerb (Verb (VerbTerm VBIPlus))) AdverbEach

