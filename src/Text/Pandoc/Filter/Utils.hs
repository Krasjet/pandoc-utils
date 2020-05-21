{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains some utility functions to work with different levels
-- of Pandoc filters. For example, for the conversion from @'Inline' ->
-- ['Inline']@ to @'Pandoc' -> 'Pandoc'@ filter.
module Text.Pandoc.Filter.Utils (
  PartialFilterM (..),
  PartialFilter,
  PandocFilterM,
  PandocFilter,
  applyFilter,
  ToPartialFilter (..)
  ) where

import Data.Functor.Identity  (Identity (..))
import Text.Pandoc.Definition
import Text.Pandoc.Walk

newtype PartialFilterM m p = PartialFilterM { applyFilterM :: p -> m p }

type PartialFilter = PartialFilterM Identity

type PandocFilterM m = PartialFilterM m Pandoc

type PandocFilter = PandocFilterM Identity

applyFilter
  :: PartialFilter p -- ^ A wrapped partial filter
  -> (p -> p)        -- ^ Unwrapped filter that can be directly applied to @p@
applyFilter = (runIdentity .) . applyFilterM

class ToPartialFilter m f p where
  toFilter
    :: f                  -- ^ A partial filter, usually @a -> a@ for some 'Walkable' @a@
    -> PartialFilterM m p -- ^ Wrapped partial Pandoc filter

instance (Monad m, Walkable a p) => ToPartialFilter m (a -> a) p where
  toFilter = PartialFilterM . (return .) . walk

instance (Monad m, Walkable a p) => ToPartialFilter m (a -> m a) p where
  toFilter = PartialFilterM . walkM

instance (Monad m, Walkable [a] p) => ToPartialFilter m (a -> [a]) p where
  toFilter = PartialFilterM . (return .) . walk . concatMap

instance (Monad m, Walkable [a] p) => ToPartialFilter m (a -> m [a]) p where
  toFilter = PartialFilterM . walkM . (fmap concat .) . mapM
