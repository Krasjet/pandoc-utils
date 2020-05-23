-- | This module contains some utility functions for building
-- attributes.
module Text.Pandoc.Filter.Utils.AttrBuilder (
  -- * Attribute builders
  addClass,
  addClasses,
  addKVPair,
  addKVPairs,
  setId,
) where

import Text.Pandoc.Utils

import Data.Bifunctor         (bimap, first, second)
import Data.Text              (Text)
import Text.Pandoc.Definition

-- | Append a new class to attributes.
addClass
  :: Attr -- ^ Original attributes.
  -> Text -- ^ Name of the class.
  -> Attr -- ^ New attributes.
attr `addClass` cls = first (fromText cls:) attr
  -- (i, fromText newCls:cls, p)

-- | Append a list of classes to attributes.
addClasses
  :: Attr   -- ^ Original attributes.
  -> [Text] -- ^ A list of class names.
  -> Attr   -- ^ New attributes.
attr `addClasses` clses = first (map fromText clses<>) attr

-- | Append a new key-value pair to attributes.
addKVPair
  :: Attr         -- ^ Original attributes.
  -> (Text, Text) -- ^ A key-value pair.
  -> Attr         -- ^ New attributes.
attr `addKVPair` p = second (bimap fromText fromText p:) attr

-- | Append new key-value pairs to attributes.
addKVPairs
  :: Attr           -- ^ Original attributes.
  -> [(Text, Text)] -- ^ A list of key-value pairs.
  -> Attr           -- ^ New attributes.
attr `addKVPairs` ps = second (map (bimap fromText fromText) ps <>) attr

-- | Set the id of attributes.
setId
  :: Attr -- ^ Original attributes.
  -> Text -- ^ The id of the attrbute.
  -> Attr -- ^ New attributes.
(_, cls, p) `setId` i = (i, cls, p)
