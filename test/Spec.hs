{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text   as T
import qualified Text.Pandoc as P

import Control.Monad.Trans.Writer
import Data.Default                         (def)
import Data.Either                          (fromRight)
import Data.Text                            (Text)
import Test.Tasty
import Test.Tasty.Hspec
import Text.Pandoc.Definition
import Text.Pandoc.Utils

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

uncapFilterInline :: Inline -> Inline
uncapFilterInline (Str str) = Str $ T.toLower str
uncapFilterInline x         = x

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

-- * Readme example

-- ** Filters

behead :: Block -> Block
behead (Header n _ xs) | n >= 2 = Para [Emph xs]
behead x               = x

beheadFilter :: PandocFilter
beheadFilter = mkFilter behead

beheadPandoc :: Pandoc -> Pandoc
beheadPandoc = convertFilter behead

delink :: Inline -> [Inline]
delink (Link _ txt _) = txt
delink x              = [x]

delinkFilter :: PandocFilter
delinkFilter = mkFilter delink

delinkPandoc :: Pandoc -> Pandoc
delinkPandoc = convertFilter delink

myFilter :: PandocFilter
myFilter = beheadFilter <> delinkFilter

-- ** Documents

readmeText :: Text
readmeText = T.strip $ T.unlines
  [ "## Heading"
  , "Hello, [Pandoc](https://pandoc.org)."
  ]

readmeDoc :: Pandoc
readmeDoc = Pandoc (Meta mempty)
  [ Header 2 ("heading", [], []) [Str "Heading"]
  , Para [ Str "Hello,"
         , Space
         , Link ("",[],[]) [Str "Pandoc"] ("https://pandoc.org","")
         , Str "."
         ]
  ]

expectedHtml :: Text
expectedHtml = T.strip $ T.unlines
  [ "<p><em>Heading</em></p>"
  , "<p>Hello, Pandoc.</p>"
  ]

expectedDoc :: Pandoc
expectedDoc = Pandoc (Meta mempty)
  [ Para [Emph [Str "Heading"]]
  , Para [ Str "Hello,"
         , Space
         , Str "Pandoc"
         , Str "."
         ]
  ]

-- ** The example

mdToHtml
  :: Text                      -- ^ Input markdown string
  -> Either P.PandocError Text -- ^ Html string or error
mdToHtml md = P.runPure $ do
  doc <- P.readMarkdown def md
  let doc' = seqFilters [beheadFilter, delinkFilter] doc
  P.writeHtml5String def doc'

mdToHtmlCompose
  :: Text                      -- ^ Input markdown string
  -> Either P.PandocError Text -- ^ Html string or error
mdToHtmlCompose md = P.runPure $ do
  doc <- P.readMarkdown def md
  let doc' = applyFilter myFilter doc
  P.writeHtml5String def doc'


convertSpec :: Spec
convertSpec = parallel $ do
  describe "mkFilter" $ do
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

    it "converts PartialFilter a to PartialFilter b" $ do
      let fInline = mkFilter capFilterInline :: PartialFilter Inline
          fBlock = mkFilter fInline          :: PartialFilter Block
          fPandoc = mkFilter fInline         :: PandocFilter
      applyFilter fBlock blockPara `shouldBe` expectedInlinePartial
      applyFilter fPandoc docPara `shouldBe` expectedInline

    it "converts PartialFilterM m a to PartialFilterM m b" $ do
      let fInline :: PartialFilterM (Writer Text) Inline
          fInline = mkFilter extractFilterInlineM
          fBlock = mkFilter fInline  :: PartialFilterM (Writer Text) Block
          fPandoc = mkFilter fInline :: PandocFilterM (Writer Text)
      let (doc, s) = runWriter $ applyFilterM fPandoc docPara
      doc `shouldBe` expectedInline
      s `shouldBe` T.pack "abcd"

      let (bl, s') = runWriter $ applyFilterM fBlock blockPara
      bl `shouldBe` expectedInlinePartial
      s' `shouldBe` T.pack "abcd"

  describe "convertFilter" $ do
    it "converts a -> a filter to Pandoc -> Pandoc filter" $ do
      convertFilter capFilterInline docPara `shouldBe` expectedInline
      convertFilter unParaFilterBlock docPara `shouldBe` expectedBlock

    it "converts a -> a filter to b -> b partial filter" $
      convertFilter capFilterInline blockPara `shouldBe` expectedInlinePartial

    it "converts a -> [a] filter to Pandoc -> Pandoc filter" $ do
      convertFilter dupFilterInline docPara `shouldBe` expectedInlineL
      convertFilter dupFilterBlock docPara `shouldBe` expectedBlockL

    it "converts a -> [a] filter to b -> b partial filter" $
      convertFilter dupFilterInline blockPara `shouldBe` expectedInlinePartialL

    it "converts a -> m a filter to Pandoc -> m Pandoc filter" $ do
      let (doc, s) = runWriter $ convertFilterM extractFilterInlineM docPara
      doc `shouldBe` expectedInline
      s `shouldBe` T.pack "abcd"

    it "converts a -> m a filter to b -> m b filter" $ do
      let (bl, s) = runWriter $ convertFilterM extractFilterInlineM blockPara
      bl `shouldBe` expectedInlinePartial
      s `shouldBe` T.pack "abcd"

    it "converts a -> m [a] filter to Pandoc -> m Pandoc filter" $ do
      let (doc, s) = runWriter $ convertFilterM extractFilterInlineML docPara
      doc `shouldBe` expectedInlineL
      s `shouldBe` T.pack "abcd"

    it "converts a -> m [a] filter to b -> m b filter" $ do
      let (bl, s) = runWriter $ convertFilterM extractFilterInlineML blockPara
      bl `shouldBe` expectedInlinePartialL
      s `shouldBe` T.pack "abcd"

  describe "getFilter" $ do
    it "converts PartialFilter a to a -> a" $ do
      let fInline = mkFilter capFilterInline :: PartialFilter Inline
      getFilter fInline (Str "abcd") `shouldBe` Str "ABCD"

    it "converts PartialFilterM m a to a -> m a" $ do
      let fInline :: PartialFilterM (Writer Text) Inline
          fInline = mkFilter extractFilterInlineM
      let (doc, s) = runWriter $ getFilterM fInline (Str "abcd")
      doc `shouldBe` Str "ABCD"
      s `shouldBe` T.pack "abcd"

    it "converts PartialFilter a to b -> b" $ do
      let fInline = mkFilter capFilterInline :: PartialFilter Inline
          fBlock = getFilter fInline         :: Block -> Block
          fPandoc = getFilter fInline        :: Pandoc -> Pandoc
      fBlock blockPara `shouldBe` expectedInlinePartial
      fPandoc docPara `shouldBe` expectedInline

    it "converts PartialFilterM m a to b -> m b" $ do
      let fInline :: PartialFilterM (Writer Text) Inline
          fInline = mkFilter extractFilterInlineM
          fBlock  = getFilterM fInline :: Block  -> Writer Text Block
          fPandoc = getFilterM fInline :: Pandoc -> Writer Text Pandoc
      let (doc, s) = runWriter $ fPandoc docPara
      doc `shouldBe` expectedInline
      s `shouldBe` T.pack "abcd"

      let (bl, s') = runWriter $ fBlock blockPara
      bl `shouldBe` expectedInlinePartial
      s' `shouldBe` T.pack "abcd"

  describe "mkConcatedFilter" $ do
    let cap = mkConcatedFilter [uncapFilterInline, capFilterInline]
        uncap = mkConcatedFilter [capFilterInline, uncapFilterInline]
    it "concats filter from left to right" $ do
      applyFilter cap docPara `shouldBe` expectedInline
      applyFilter uncap docPara `shouldBe` docPara

  describe "toFilterM" $
    it "converts a -> a filter to a -> m a filter" $ do
      let capFilterM = toFilterM $ mkFilter capFilterInline
      applyFilterM capFilterM (Str "abcd") `shouldBe` Just (Str "ABCD")

composeSpec :: Spec
composeSpec = parallel $ do
  let dup = mkFilter dupFilterInline
      merge = mkFilter mergeFilterInline
      extract = mkFilter extractFilter
  describe "applyFilter" $ do
    it "applys dup correctly" $
      applyFilter dup compPara `shouldBe` expectedDup

    it "applys merge correctly" $
      applyFilter merge compPara2 `shouldBe` expectedMerge

  describe "seqFilters" $ do
    it "applys PartialFilter composition from left to right" $ do
      applyFilter (dup <> merge) compPara `shouldBe` expectedDupMerge
      applyFilter (merge <> dup) compPara `shouldBe` expectedMergeDup

    it "applys PartialFilterM composition from left to right" $ do
      let (doc, s) = runWriter $ applyFilterM (extract <> toFilterM dup) compPara
      doc `shouldBe` expectedDup
      s `shouldBe` T.pack "abcd"

      let (doc', s') = runWriter $ applyFilterM (toFilterM dup <> extract) compPara
      doc' `shouldBe` expectedDup
      s' `shouldBe` T.pack "abcdabcd"

  describe "monoid instance" $ do
    it "applys PartialFilter composition from left to right" $ do
      seqFilters [dup, merge] compPara `shouldBe` expectedDupMerge
      seqFilters [merge, dup] compPara `shouldBe` expectedMergeDup

    it "applys PartialFilterM composition from left to right" $ do
      let (doc, s) = runWriter $ seqFiltersM [extract, toFilterM dup] compPara
      doc `shouldBe` expectedDup
      s `shouldBe` T.pack "abcd"

      let (doc', s') = runWriter $ seqFiltersM [toFilterM dup, extract] compPara
      doc' `shouldBe` expectedDup
      s' `shouldBe` T.pack "abcdabcd"

  let dupInl = mkFilter dupFilterInline     :: PartialFilter [Inline]
      mergeInl = mkFilter mergeFilterInline :: PartialFilter [Inline]
      extractInl = mkFilter extractFilter   :: PartialFilterM (Writer Text) [Inline]

  describe "getConcatedFilter" $ do
    it "converts [PartialFilter a] to a -> a, applied from left to right" $ do
      getConcatedFilter [dupInl, mergeInl] [Str "abcd"] `shouldBe` [Str "abcdabcd"]
      getConcatedFilter [mergeInl, dupInl] [Str "abcd"] `shouldBe` [Str "abcd", Str "abcd"]

    it "converts [PartialFilterM m a] to a -> m a, applied from left to right" $ do
      let (doc, s) = runWriter $ getConcatedFilterM [extractInl, toFilterM dupInl] [Str "abcd"]
      doc `shouldBe` [Str "abcd", Str "abcd"]
      s `shouldBe` T.pack "abcd"

      let (doc', s') = runWriter $ getConcatedFilterM [toFilterM dupInl, extractInl] [Str "abcd"]
      doc' `shouldBe` [Str "abcd", Str "abcd"]
      s' `shouldBe` T.pack "abcdabcd"

    it "converts [PartialFilter a] to b -> b, applied from left to right" $ do
      getConcatedFilter [dupInl, mergeInl] compPara `shouldBe` expectedDupMerge
      getConcatedFilter [mergeInl, dupInl] compPara `shouldBe` expectedMergeDup

    it "converts [PartialFilterM m a] to b -> m b, applied from left to right" $ do
      let (doc, s) = runWriter $ getConcatedFilterM [extractInl, toFilterM dupInl] compPara
      doc `shouldBe` expectedDup
      s `shouldBe` T.pack "abcd"

      let (doc', s') = runWriter $ getConcatedFilterM [toFilterM dupInl, extractInl] compPara
      doc' `shouldBe` expectedDup
      s' `shouldBe` T.pack "abcdabcd"

readmeSpec :: Spec
readmeSpec = parallel $
  describe "readme example" $ do
    it "processes filter examples correctly on AST level" $ do
      seqFilters [beheadFilter, delinkFilter] readmeDoc `shouldBe` expectedDoc
      applyFilter myFilter readmeDoc `shouldBe` expectedDoc
      (delinkPandoc . beheadPandoc) readmeDoc `shouldBe` expectedDoc

    it "processes filter examples correctly on Text level" $ do
      fromRight "" (mdToHtml readmeText) `shouldBe` expectedHtml
      fromRight "" (mdToHtmlCompose readmeText) `shouldBe` expectedHtml

attrBuilderSpec :: Spec
attrBuilderSpec = parallel $ do
  let testAttr = ("id", ["test"], [("k", "v")])
  describe "addClass" $
    it "adds a new class to attributes" $ do
      nullAttr `addClass` "test" `shouldBe` ("", ["test"], [])
      nullAttr `addClass` "test" `addClass` "test2" `shouldBe` ("", ["test2", "test"], [])
      testAttr `addClass` "test2" `shouldBe` ("id", ["test2", "test"], [("k", "v")])

  describe "addClasses" $
    it "adds new classes to attributes" $ do
      nullAttr `addClasses` ["test", "test2"] `shouldBe` ("", ["test", "test2"], [])
      testAttr `addClasses` ["test1", "test2"] `shouldBe` ("id", ["test1", "test2", "test"], [("k", "v")])

  describe "addKVPair" $
    it "adds a new kv pair to attributes" $ do
      nullAttr `addKVPair` ("k", "v") `shouldBe` ("", [], [("k", "v")])
      testAttr `addKVPair` ("k2", "v2") `shouldBe` ("id", ["test"], [("k2", "v2"), ("k", "v")])

  describe "addKVPairs" $
    it "adds new kv pairs to attributes" $ do
      nullAttr `addKVPairs` [("k", "v"), ("k2", "v2")] `shouldBe` ("", [], [("k", "v"), ("k2", "v2")])
      testAttr `addKVPairs` [("k1", "v1"), ("k2", "v2")] `shouldBe` ("id", ["test"], [("k1", "v1"), ("k2", "v2"), ("k", "v")])

  describe "setId" $
    it "sets id of attributes" $ do
      nullAttr `setId` "id" `shouldBe` ("id", [], [])
      testAttr `setId` "id2" `shouldBe` ("id2", ["test"], [("k", "v")])

main :: IO ()
main = do
  testConvert <- testSpec "Filter conversion" convertSpec
  testCompose <- testSpec "Filter composition" composeSpec
  testReadme <- testSpec "Readme examples" readmeSpec
  testAttrBuilder <- testSpec "Attr builder" attrBuilderSpec
  defaultMain $ testGroup "Tests"
    [ testConvert
    , testCompose
    , testReadme
    , testAttrBuilder
    ]
