{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains some utility functions to work with different levels
-- of Pandoc filters. For example, for the conversion from @'Inline' ->
-- ['Inline']@ to @'Pandoc' -> 'Pandoc'@ filter.
module Text.Pandoc.Filter.Utils (
  PartialFilterM (applyFilterM),
  PartialFilter,
  PandocFilterM,
  PandocFilter,
  applyFilter,
  getFilterM,
  getFilter,
  toFilterM,
  applyFiltersM,
  applyFilters,
  ToPartialFilter (..)
  ) where

import Control.Monad          ((>=>))
import Data.Foldable          (fold)
import Data.Functor.Identity  (Identity (..))
import Text.Pandoc.Definition
import Text.Pandoc.Walk

-- | @PartialFilterM m p@ is a wrapper for any monadic @p -> m p@ Pandoc
-- filters acting on a subnode (e.g. 'Inline' or 'Block') of the 'Pandoc'
-- abstract syntax tree.
--
-- * @m@: a monad.
-- * @p@: the type of a subnode of 'Pandoc' (e.g. 'Inline').
newtype PartialFilterM m p =
  PartialFilterM
    { -- | Apply the filter on @p@.
      applyFilterM :: p -> m p
    }

-- | @PartialFilter p@ is a wrapper for ordinary @p -> p@ Pandoc filters acting
-- on a subnode (e.g. 'Inline' or 'Block') of the 'Pandoc' abstract syntax
-- tree.
--
-- * @p@: the type of a subnode of 'Pandoc' (e.g. 'Inline').
type PartialFilter = PartialFilterM Identity

-- | A synonym for @PartialFilter Pandoc@. It encapsulates a monadic
-- filter @'Pandoc' -> m 'Pandoc'@ acting directly on 'Pandoc'.
--
-- * @m@: a monad.
type PandocFilter = PartialFilter Pandoc

-- | A synonym for @PartialFilterM m Pandoc@, a monadic version of
-- 'PandocFilter'.
--
-- * @m@: a monad.
type PandocFilterM m = PartialFilterM m Pandoc

applyFilter
  :: PartialFilter p -- ^ A wrapped partial filter
  -> (p -> p)        -- ^ Unwrapped filter that can be directly applied to @p@
applyFilter = (runIdentity .) . applyFilterM

-- | A synonym for 'applyFilterM'. It can be used when you don't need to apply
-- the filter immediately.
getFilterM
  :: PartialFilterM m p -- ^ A wrapped partial filter
  -> (p -> m p)        -- ^ Unwrapped filter that can be directly applied to @p@
getFilterM = applyFilterM

-- | A synonym for 'applyFilter'. It can be used when you don't need to apply
-- the filter immediately.
getFilter
  :: PartialFilter p -- ^ A wrapped partial filter
  -> (p -> p)        -- ^ Unwrapped filter that can be directly applied to @p@
getFilter = applyFilter

instance (Monad m) => Semigroup (PartialFilterM m p) where
  f1 <> f2 = PartialFilterM (applyFilterM f1 >=> applyFilterM f2)

instance (Monad m) => Monoid (PartialFilterM m p) where
  mempty = PartialFilterM return

class ToPartialFilter m f p where
  mkFilter
    :: f                  -- ^ A partial filter, usually @a -> a@ for some 'Walkable' @a@
    -> PartialFilterM m p -- ^ Wrapped partial Pandoc filter

instance (Monad m, Walkable a p) => ToPartialFilter m (a -> a) p where
  mkFilter = PartialFilterM . (return .) . walk

instance (Monad m, Walkable a p) => ToPartialFilter m (a -> m a) p where
  mkFilter = PartialFilterM . walkM

instance (Monad m, Walkable [a] p) => ToPartialFilter m (a -> [a]) p where
  mkFilter = PartialFilterM . (return .) . walk . concatMap

instance (Monad m, Walkable [a] p) => ToPartialFilter m (a -> m [a]) p where
  mkFilter = PartialFilterM . walkM . (fmap concat .) . mapM

-- | Convert an ordinary 'PartialFilter' to the monadic version
-- 'PartialFilterM'.
toFilterM
  :: (Monad m)
  => PartialFilter a    -- ^ An ordinary filter.
  -> PartialFilterM m a -- ^ The monadic version.
toFilterM = PartialFilterM . (return .) . getFilter

-- | A apply a list of monadic partial filters sequentially, from left to
-- right, i.e.  the first element in the list will be applied first and the
-- last element will be applied at the end.
applyFiltersM
  :: (Foldable t, Monad m)
  => t (PartialFilterM m p) -- ^ A list of monadic partial filters
  -> (p -> m p)             -- ^ Unwrapped monadic filter applicable to @p@ directly
applyFiltersM = getFilterM . fold

-- | A apply a list of partial filters sequentially, from left to right, i.e.
-- the first element in the list will be applied first and the last element
-- will be applied at the end.
applyFilters
  :: (Foldable t)
  => t (PartialFilter p) -- ^ A list of partial filter.
  -> (p -> p)          -- ^ Unwrapped filter applicable to @p@ directly
applyFilters = getFilter . fold
