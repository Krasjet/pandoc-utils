{-# LANGUAGE TypeFamilies #-}

-- | Due to the switch from 'String' to 'Text' in pandoc-types 1.20, some
-- legacy filters might not work in newer versions. This module contains some
-- string conversion utility functions to make filters work in across different
-- versions.
module Text.Pandoc.Utils (
  -- * Conversions
  ToString (..),
  ToText (..),
  fromString,
  ) where

import qualified Data.Text as T

import Data.String (IsString (..))
import Data.Text   (Text)

-- | A helper typeclass for converting 'String' and 'Text' to 'String'.
class IsString s => ToString s where
  -- | Convert strings to 'String'.
  toString :: s -> String

instance Char ~ c => ToString [c] where
  toString = id

instance ToString Text where
  toString = T.unpack

-- | A helper typeclass for converting 'String' and 'Text' to 'Text'.
class IsString s => ToText s where
  -- | Convert strings to 'Text'.
  toText :: s -> Text

instance Char ~ c => ToText [c] where
  toText = T.pack

instance ToText Text where
  toText = id
