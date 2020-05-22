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

-- ** Composition results

compPara :: Block
compPara = Para [Str "abcd"]

compPara2 :: Block
compPara2 = Para [Str "abcd", Str "efgh"]

expectedDup :: Block
expectedDup = Para [Str "abcd", Str "abcd"]

expectedMerge :: Block
expectedMerge = Para [Str "abcdefgh"]

expectedDupMerge :: Block
expectedDupMerge = Para [Str "abcdabcd"]

expectedMergeDup :: Block
expectedMergeDup = Para [Str "abcd", Str "abcd"]

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

mergeFilterInline :: [Inline] -> [Inline]
mergeFilterInline (Str str1 : Str str2 : xs) = Str (str1 <> str2) : xs
mergeFilterInline x                          = x

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

extractFilter :: Inline -> Writer Text Inline
extractFilter il@(Str str) = tell str >> return il
extractFilter x            = return x

convertSpec :: Spec
convertSpec = parallel $ do
  it "converts a -> a filter to Pandoc -> Pandoc filter" $ do
    applyFilter (mkFilter capFilterInline) docPara `shouldBe` expectedInline
    applyFilter (mkFilter unParaFilterBlock) docPara `shouldBe` expectedBlock

  it "converts a -> a filter to b -> b partial filter" $
    applyFilter (mkFilter capFilterInline) blockPara `shouldBe` expectedInlinePartial

  it "converts a -> [a] filter to Pandoc -> Pandoc filter" $ do
    applyFilter (mkFilter dupFilterInline) docPara `shouldBe` expectedInlineL
    applyFilter (mkFilter dupFilterBlock) docPara `shouldBe` expectedBlockL

  it "converts a -> [a] filter to b -> b partial filter" $
    applyFilter (mkFilter dupFilterInline) blockPara `shouldBe` expectedInlinePartialL

  it "converts a -> m a filter to Pandoc -> m Pandoc filter" $ do
    let (doc, s) = runWriter $ applyFilterM (mkFilter extractFilterInlineM) docPara
    doc `shouldBe` expectedInline
    s `shouldBe` T.pack "abcd"

  it "converts a -> m a filter to b -> m b filter" $ do
    let (bl, s) = runWriter $ applyFilterM (mkFilter extractFilterInlineM) blockPara
    bl `shouldBe` expectedInlinePartial
    s `shouldBe` T.pack "abcd"

  it "converts a -> m [a] filter to Pandoc -> m Pandoc filter" $ do
    let (doc, s) = runWriter $ applyFilterM (mkFilter extractFilterInlineML) docPara
    doc `shouldBe` expectedInlineL
    s `shouldBe` T.pack "abcd"

  it "converts a -> m [a] filter to b -> m b filter" $ do
    let (bl, s) = runWriter $ applyFilterM (mkFilter extractFilterInlineML) blockPara
    bl `shouldBe` expectedInlinePartialL
    s `shouldBe` T.pack "abcd"

  it "converts a -> a filter to a -> m a filter" $ do
    let capFilterM = toFilterM $ mkFilter capFilterInline
    applyFilterM capFilterM (Str "abcd") `shouldBe` Just (Str "ABCD")

composeSpec :: Spec
composeSpec = parallel $ do
  let dup = mkFilter dupFilterInline
      merge = mkFilter mergeFilterInline
      extract = mkFilter extractFilter
  it "applys dup correctly" $
    applyFilter dup compPara `shouldBe` expectedDup

  it "applys merge correctly" $
    applyFilter merge compPara2 `shouldBe` expectedMerge

  it "applys PartialFilter composition from left to right" $ do
    applyFilter (dup <> merge) compPara `shouldBe` expectedDupMerge
    applyFilter (merge <> dup) compPara `shouldBe` expectedMergeDup
    applyFilters [dup, merge] compPara `shouldBe` expectedDupMerge
    applyFilters [merge, dup] compPara `shouldBe` expectedMergeDup

  it "applys PartialFilterM composition from left to right" $ do
    let (doc, s) = runWriter $ applyFilterM (extract <> toFilterM dup) compPara
    doc `shouldBe` expectedDup
    s `shouldBe` T.pack "abcd"

    let (doc', s') = runWriter $ applyFilterM (toFilterM dup <> extract) compPara
    doc' `shouldBe` expectedDup
    s' `shouldBe` T.pack "abcdabcd"

main :: IO ()
main = do
  testConvert <- testSpec "Filter conversion" convertSpec
  testCompose <- testSpec "Filter composition" composeSpec
  defaultMain $ testGroup "Tests"
    [ testConvert
    , testCompose
    ]
