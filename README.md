# pandoc-utils

This package contains some useful functions for writing
[Pandoc](https://pandoc.org/) filters and integrating Pandoc into Haskell
applications such as [Hakyll](https://jaspervdj.be/hakyll/). It provides a
composable wrapper for filters acting on nodes of the [Pandoc
AST](https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html).

## Filter conversion/composition

As an example, let us look at the `behead` and `delink` filter from [Pandoc's
tutorial](https://pandoc.org/filters.html).
```haskell
behead :: Block -> Block
behead (Header n _ xs) | n >= 2 = Para [Emph xs]
behead x = x

delink :: Inline -> [Inline]
delink (Link _ txt _) = txt
delink x = [x]
```

Since `behead` has type `Block -> Block`, while `delink` has type `Inline ->
[Inline]`, they are not naturally composable. However, this package provides a
utility function `mkFilter` to convert them into a `PandocFilter`.
```haskell
import Text.Pandoc.Filter.Utils

beheadFilter :: PandocFilter
beheadFilter = mkFilter behead

delinkFilter :: PandocFilter
delinkFilter = mkFilter delink
```
`PandocFilter` is an alias for `PartialFilter Pandoc`, so you can also have
`PartialFilter Inline`, etc. There is also a monadic version called
`PartialFilterM` for encapsulating monadic filters.

The `PandocFilter` is a monoid so you can do something like,
```haskell
myFilter :: PandocFilter
myFilter = beheadFilter <> delinkFilter
```
where `myFilter` would apply `beheadFilter` first, then the `delinkFilter`. You
can apply the filter using `applyFilter`,
```haskell
import Text.Pandoc
import Data.Default (def)

mdToHtml
  :: Text                    -- ^ Input markdown string
  -> Either PandocError Text -- ^ Html string or error
mdToHtml md = runPure $ do
  doc <- readMarkdown def md
  let doc' = applyFilter myFilter doc
  writeHtml5String def doc'
```
or get an unwrapped `Pandoc -> Pandoc` filter using `getFilter` (this function
is also capable of doing implicit conversion from `PartialFilter a` to `b ->
b`).
```haskell
myPandocFilter :: Pandoc -> Pandoc
myPandocFilter = getFilter myFilter
```

For applying multiple filters, there is also a function called `applyFilters`,
which takes a list of filters and apply it to a `Pandoc` document (or subnode)
sequentially, from left to right.
```haskell
myFilters :: [PandocFilter]
myFilters =
  [ beheadFilter
  , delinkFilter
  ]

mdToHtml'
  :: Text                    -- ^ Input markdown string
  -> Either PandocError Text -- ^ Html string or error
mdToHtml' md = runPure $ do
  doc <- readMarkdown def md
  let doc' = applyFilters myFilters doc
  writeHtml5String def doc'
```

## Attribute builder

pandoc-utils also provides an attribute builder for handling attributes. You
can create a new attributes by
```haskell
ghci> import Text.Pandoc.Filter.Utils.AttrBuilder
ghci> import Text.Pandoc.Definition
ghci> nullAttr `setId` "id" `addClass` "class" `addKVPair` ("key","value")
("id",["class"],[("key","value")])
```
or modifying an existing attributes by
```haskell
ghci> attr = ("id",[],[])
ghci> attr `setId` "newId"
("newId",[],[])
```

For more examples, please read the
[spec](https://github.com/Krasjet/pandoc-utils/blob/master/test/Spec.hs).
