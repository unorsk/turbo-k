module Main (main) where

import Parserq (Expr (..), QOperation (..), QTerm (..), parseq)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = hspec $
  describe "parseq" $ do
    describe "simple symbol" $ do
      it "Parses symbol" $
        parse parseq "" "`abc" `shouldParse` Term (QSymbol "abc")
    describe "simple int" $ do
      it "Parses int" $
        parse parseq "" "1" `shouldParse` Term (QInt 1)
    describe "int addition" $ do
      it "Sum ints" $
        parse parseq "" "1+1"
          `shouldParse` Expr (QInt 1) Add (QInt 1)