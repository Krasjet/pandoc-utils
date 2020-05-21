{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

import Control.Monad.Trans.Writer
import Data.Text                  (Text)
import Test.Tasty
import Test.Tasty.Hspec
import Text.Pandoc.Definition
import Text.Pandoc.Filter.Utils

-- * Testing data

-- ** Partial level

blockPara :: Block
blockPara = Para [Str "abcd"]

expectedInlinePartial :: Block
expectedInlinePartial = Para [Str "ABCD"]

expectedInlinePartialL :: Block
expectedInlinePartialL = Para [Str "abcd", Str "abcd"]

-- ** Pandoc level

docPara :: Pandoc
docPara = Pandoc (Meta mempty) [blockPara]

expectedInline :: Pandoc
expectedInline = Pandoc (Meta mempty) [Para [Str "ABCD"]]

expectedBlock :: Pandoc
expectedBlock = Pandoc (Meta mempty) [Plain [Str "abcd"]]

expectedInlineL :: Pandoc
expectedInlineL = Pandoc (Meta mempty) [Para [Str "abcd", Str "abcd"]]

expectedBlockL :: Pandoc
expectedBlockL = Pandoc (Meta mempty) [blockPara, blockPara]

-- * Filters

-- ** Plain filters

capFilterInline :: Inline -> Inline
capFilterInline (Str str) = Str $ T.toUpper str
capFilterInline x         = x

unParaFilterBlock :: Block -> Block
unParaFilterBlock (Para ils) = Plain ils
unParaFilterBlock x          = x

dupFilterInline :: Inline -> [Inline]
dupFilterInline (Str str) = [Str str, Str str]
dupFilterInline x         = [x]

dupFilterBlock :: Block -> [Block]
dupFilterBlock (Para ils) = [Para ils, Para ils]
dupFilterBlock x          = [x]

-- ** Monadic filters

extractFilterInlineM :: Inline -> Writer Text Inline
extractFilterInlineM il@(Str str) = tell str >> return (capFilterInline il)
extractFilterInlineM x            = return x

extractFilterInlineML :: Inline -> Writer Text [Inline]
extractFilterInlineML il@(Str str) = tell str >> return (dupFilterInline il)
extractFilterInlineML x            = return [x]

filterSpec :: Spec
filterSpec = parallel $ do
  it "converts a -> a filter to Pandoc -> Pandoc filter" $ do
    applyFilter (toFilter capFilterInline) docPara `shouldBe` expectedInline
    applyFilter (toFilter unParaFilterBlock) docPara `shouldBe` expectedBlock

  it "converts a -> a filter to b -> b partial filter" $
    applyFilter (toFilter capFilterInline) blockPara `shouldBe` expectedInlinePartial

  it "converts a -> [a] filter to Pandoc -> Pandoc filter" $ do
    applyFilter (toFilter dupFilterInline) docPara `shouldBe` expectedInlineL
    applyFilter (toFilter dupFilterBlock) docPara `shouldBe` expectedBlockL

  it "converts a -> [a] filter to b -> b partial filter" $
    applyFilter (toFilter dupFilterInline) blockPara `shouldBe` expectedInlinePartialL

  it "converts a -> m a filter to Pandoc -> m Pandoc filter" $ do
    let (doc, s) = runWriter $ applyFilterM (toFilter extractFilterInlineM) docPara
    doc `shouldBe` expectedInline
    s `shouldBe` T.pack "abcd"

  it "converts a -> m a filter to b -> m b filter" $ do
    let (bl, s) = runWriter $ applyFilterM (toFilter extractFilterInlineM) blockPara
    bl `shouldBe` expectedInlinePartial
    s `shouldBe` T.pack "abcd"

  it "converts a -> m [a] filter to Pandoc -> m Pandoc filter" $ do
    let (doc, s) = runWriter $ applyFilterM (toFilter extractFilterInlineML) docPara
    doc `shouldBe` expectedInlineL
    s `shouldBe` T.pack "abcd"

  it "converts a -> m [a] filter to b -> m b filter" $ do
    let (bl, s) = runWriter $ applyFilterM (toFilter extractFilterInlineML) blockPara
    bl `shouldBe` expectedInlinePartialL
    s `shouldBe` T.pack "abcd"

main :: IO ()
main = do
  test <- testSpec "filter conversion" filterSpec
  defaultMain test
