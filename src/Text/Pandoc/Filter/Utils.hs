{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains some utility functions to work with different levels
-- of Pandoc filters. For example, for the conversion from @'Inline' ->
-- ['Inline']@ to @'Pandoc' -> 'Pandoc'@ filter.
module Text.Pandoc.Filter.Utils (
  -- * Definitions
  PartialFilterM,
  PartialFilter,
  PandocFilterM,
  PandocFilter,
  -- * Filter application
  applyFilterM,
  applyFilter,
  -- * Filter composition
  applyFiltersM,
  applyFilters,
  -- * Filter conversion
  getFilterM,
  getFilter,
  getConcatedFilterM,
  getConcatedFilter,
  ToPartialFilter (..),
  mkConcatedFilter,
  toFilterM,
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
    { -- | Apply a monadic filter to @p@.
      applyFilterM :: p -> m p
    }

-- | @PartialFilter p@ is a wrapper for ordinary @p -> p@ Pandoc filters acting
-- on a subnode (e.g. 'Inline' or 'Block') of the 'Pandoc' abstract syntax
-- tree.
--
-- * @p@: the type of a subnode of 'Pandoc' (e.g. 'Inline').
type PartialFilter = PartialFilterM Identity

-- | An alias for @PartialFilter Pandoc@. It encapsulates a monadic
-- filter @'Pandoc' -> m 'Pandoc'@ acting directly on 'Pandoc'.
--
-- * @m@: a monad.
type PandocFilter = PartialFilter Pandoc

-- | An alias for @PartialFilterM m Pandoc@, a monadic version of
-- 'PandocFilter'.
--
-- * @m@: a monad.
type PandocFilterM m = PartialFilterM m Pandoc

-- | Apply an ordinary filter to @p@, which returns @p@ directly.
applyFilter
  :: PartialFilter p -- ^ A wrapped partial filter.
  -> (p -> p)        -- ^ Unwrapped filter that can be directly applied to @p@.
applyFilter = (runIdentity .) . applyFilterM

-- | An alias for 'applyFilterM'. It can be used when you don't need to apply
-- the filter immediately.
getFilterM
  :: PartialFilterM m p -- ^ A wrapped partial filter.
  -> (p -> m p)         -- ^ Unwrapped filter that can be directly applied to @p@.
getFilterM = applyFilterM

-- | An alias for 'applyFilter'. It can be used when you don't need to apply
-- the filter immediately.
getFilter
  :: PartialFilter p -- ^ A wrapped partial filter.
  -> (p -> p)        -- ^ Unwrapped filter that can be directly applied to @p@.
getFilter = applyFilter

-- | The 'Semigroup' instance of `PartialFilterM`. @f1 <> f2@ will apply @f1@
-- first followed by @f2@.
instance (Monad m) => Semigroup (PartialFilterM m p) where
  f1 <> f2 = PartialFilterM (applyFilterM f1 >=> applyFilterM f2)

-- | The 'Monoid' instance of `PartialFilterM`.
instance (Monad m) => Monoid (PartialFilterM m p) where
  mempty = PartialFilterM return

-- | A helper typeclass used as a polymorphic constructor of 'PartialFilterM'.
class ToPartialFilter m f p where
  -- | The actual constructor of 'PartialFilterM'. It takes an ordinary filter
  -- function @a -> b@ and wraps it as a 'PartialFilterM'. It can also be used
  -- to convert between different types of @'PartialFilterM' m@.
  mkFilter
    :: f                  -- ^ A partial filter function, usually @a -> a@ for some @'Walkable' a p@.
    -> PartialFilterM m p -- ^ Wrapped partial Pandoc filter.

instance (Monad m, Walkable a p) => ToPartialFilter m (a -> a) p where
  mkFilter = PartialFilterM . (return .) . walk

instance (Monad m, Walkable a p) => ToPartialFilter m (a -> m a) p where
  mkFilter = PartialFilterM . walkM

instance (Monad m, Walkable [a] p) => ToPartialFilter m (a -> [a]) p where
  mkFilter = PartialFilterM . (return .) . walk . concatMap

instance (Monad m, Walkable [a] p) => ToPartialFilter m (a -> m [a]) p where
  mkFilter = PartialFilterM . walkM . (fmap concat .) . mapM

-- | This instance can be used to convert @'PartialFilterM' m a@ to
-- @'PartialFilterM' m b@.
instance (Monad m, Walkable a b) => ToPartialFilter m (PartialFilterM m a) b where
  mkFilter = mkFilter . getFilterM

-- | Construct a 'PartialFilterM' from a list of filter functions of the same
-- type. The final filter is concatenated from left to right such that the
-- first element in the list will be applied first and the last element will be
-- applied at the end.
mkConcatedFilter
  :: (Monad m, ToPartialFilter m f p, Foldable t)
  => t f                -- ^ A list of filter functions of the same type.
  -> PartialFilterM m p -- ^ Concatenated filter.
mkConcatedFilter = foldMap mkFilter

-- | Convert an ordinary 'PartialFilter' to the monadic version
-- 'PartialFilterM'.
toFilterM
  :: (Monad m)
  => PartialFilter p    -- ^ An ordinary filter.
  -> PartialFilterM m p -- ^ The monadic version.
toFilterM = PartialFilterM . (return .) . getFilter

-- convertFilter
--   :: (Monad m, ToPartialFilter m a b)
--   => PartialFilterM m a    -- ^ An ordinary filter.
--   -> PartialFilterM m b -- ^ The monadic version.
-- convertFilter = mkFilter . getFilterM

-- | Apply a list of monadic partial filters sequentially, from left to
-- right, i.e.  the first element in the list will be applied first and the
-- last element will be applied at the end.
applyFiltersM
  :: (Foldable t, Monad m)
  => t (PartialFilterM m p) -- ^ A list of monadic partial filters.
  -> (p -> m p)             -- ^ Unwrapped monadic filter applicable to @p@ directly.
applyFiltersM = getFilterM . fold

-- | Apply a list of partial filters sequentially, from left to right, i.e.
-- the first element in the list will be applied first and the last element
-- will be applied at the end.
applyFilters
  :: (Foldable t)
  => t (PartialFilter p) -- ^ A list of partial filter.
  -> (p -> p)            -- ^ Unwrapped filter applicable to @p@ directly.
applyFilters = getFilter . fold

-- | An alias for 'applyFiltersM', used when the filter is not used
-- immediately.
getConcatedFilterM
  :: (Foldable t, Monad m)
  => t (PartialFilterM m p) -- ^ A list of monadic partial filters.
  -> (p -> m p)             -- ^ Unwrapped monadic filter applicable to @p@ directly.
getConcatedFilterM = applyFiltersM

-- | An alias for 'applyFilters', used when the filter is not used immediately.
getConcatedFilter
  :: (Foldable t)
  => t (PartialFilter p) -- ^ A list of partial filter.
  -> (p -> p)            -- ^ Unwrapped filter applicable to @p@ directly.
getConcatedFilter = applyFilters
