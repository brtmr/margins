module Spec where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT

import Annotate
import Parser

main :: IO ()
main = hspec $ spec

simple_annotation :: Annotation
simple_annotation = 
    let 
        loc = Lines 3 3
        identifier = Id $ T.pack "7535423c-97ad-4102-afd1-a2647e1de17b"
        lines = T.concat [ T.pack "These \n", T.pack "are \n", T.pack "lines \n" ]
    in
        Annotation loc identifier lines


spec :: Spec
spec = do
    describe "Parser" $ do
        it "decodes 3 lines" $ 
            let 
                value = case (AT.parse (nlines 3) (T.pack "these\nare\nlines\n")) of 
                    AT.Done i r -> (i, r)
                    _ -> (T.pack "", T.pack "parse failed.")
                expectation = (T.pack "", T.pack "these\nare\nlines\n")
            in value `shouldBe` expectation
    describe "Annotations" $ do
        it "encode / decode is idempotent" $ 
            (fromText (astext simple_annotation)) `shouldBe` (Just simple_annotation)

