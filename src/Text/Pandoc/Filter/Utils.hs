{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains some utility functions to work with different levels
-- of Pandoc filters. For example, for the conversion from @'Inline' ->
-- ['Inline']@ to @'Pandoc' -> 'Pandoc'@ filter.
--
-- If you don't need to compose filters and only want to convert between Pandoc
-- filter functions, just use 'convertFilter' and 'convertFilterM'.
--
-- However, if you are working with multiple Pandoc filters of different type
-- and want to compose them, this module also provides a monoid wrapper type
-- 'PartialFilterM', which I call a "wrapped filter", and a few functions to
-- apply, compose, and convert them.
module Text.Pandoc.Filter.Utils (
  -- * Filter function conversion
  convertFilter,
  convertFilterM,
  -- * Wrapped filter definitions
  PartialFilter,
  PandocFilter,
  PartialFilterM,
  PandocFilterM,
  -- * Wrapped filter application/composition
  applyFilter,
  applyFilters,
  applyFilterM,
  applyFiltersM,
  -- * Wrapped filter → filter function
  getFilter,
  getConcatedFilter,
  getFilterM,
  getConcatedFilterM,
  -- * Wrapped filter conversion
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
-- abstract syntax tree. On this page, we will call it a "wrapped" filter to
-- distinguish it from filter functions @a -> m b@.
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
-- tree. On this page, we will call it a "wrapped" filter to distinguish it
-- from filter functions @a -> b@.
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

-- | Apply a wrapped filter to @p@, which returns @p@ directly.
applyFilter
  :: PartialFilter p -- ^ A wrapped filter.
  -> p               -- ^ 'Pandoc' AST node.
  -> p               -- ^ Transformed node.
applyFilter = (runIdentity .) . applyFilterM

-- | It is mostly the same as 'applyFilterM', which converts a wrapped monadic
-- filter to a monadic filter function, but it should be used when you don't
-- need to apply the filter immediately. There is a slight difference in that
-- it will perform an implicit conversion if the requested filter function is
-- of a different type.
--
-- For example, it can be used to convert a wrapped monadic filter
-- @'PartialFilterM' 'IO' 'Inline'@ to monadic filter function @'Block' -> 'IO'
-- 'Block'@.
getFilterM
  :: (Monad m, Walkable a b)
  => PartialFilterM m a -- ^ A wrapped filter on @a@.
  -> (b -> m b)         -- ^ Filter function that can be directly applied to @b@.
getFilterM = applyFilterM . mkFilter

-- | It is mostly the same as 'applyFilter', which converts a wrapped filter to
-- a filter function, but it should be used when you don't need to apply the
-- filter immediately. There is a slight difference in that it will perform an
-- implicit conversion if the requested filter function is of a different type.
--
-- For example, it can be used to convert a wrapped filter @'PartialFilter'
-- 'Inline'@ to filter function @'Block' -> 'Block'@.
getFilter
  :: (Walkable a b)
  => PartialFilter a -- ^ A wrapped partial filter on @a@.
  -> (b -> b)        -- ^ Filter function that can be directly applied to @b@.
getFilter = applyFilter . mkFilter

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
  -- function @a -> b@ and wraps it as a wrapped filter 'PartialFilterM'. It
  -- can also be used to convert between different types of @'PartialFilterM'
  -- m@.
  mkFilter
    :: f                  -- ^ A filter function, usually @a -> a@ for some @'Walkable' a p@.
    -> PartialFilterM m p -- ^ Wrapped Pandoc filter.

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
  mkFilter = mkFilter . applyFilterM

-- | Construct a wrapped filter 'PartialFilterM' from a list of filter
-- functions of the same type. The final filter is concatenated from left to
-- right such that the first element in the list will be applied first and the
-- last element will be applied at the end.
--
-- For example, it can be used to convert an list of filter functions
-- @['Inline' -> ['Inline']]@ to a wrapped filter @'PandocFilter'@.
mkConcatedFilter
  :: (Monad m, ToPartialFilter m f p, Foldable t)
  => t f                -- ^ A list of filter functions of the same type.
  -> PartialFilterM m p -- ^ Concatenated filter.
mkConcatedFilter = foldMap mkFilter

-- | Convert an ordinary wrapped filter 'PartialFilter' to the monadic version
-- 'PartialFilterM'.
--
-- For example, it can be used to convert an ordinary wrapped filter
-- @'PartialFilter' 'Inline'@ to monadic wrapped filter @'PartialFilterM' 'IO'
-- 'Inline'@.
toFilterM
  :: (Monad m)
  => PartialFilter p    -- ^ An ordinary filter.
  -> PartialFilterM m p -- ^ The monadic version.
toFilterM = PartialFilterM . (return .) . applyFilter

-- | Apply a list of monadic wrapped filters sequentially, from left to right,
-- i.e. the first element in the list will be applied first and the last
-- element will be applied at the end.
applyFiltersM
  :: (Foldable t, Monad m)
  => t (PartialFilterM m p) -- ^ A list of monadic wrapped filters.
  -> p                      -- ^ 'Pandoc' AST node.
  -> m p                    -- ^ Transformed node.
applyFiltersM = applyFilterM . fold

-- | Apply a list of wrapped filters sequentially, from left to right, i.e.
-- the first element in the list will be applied first and the last element
-- will be applied at the end.
applyFilters
  :: (Foldable t)
  => t (PartialFilter p) -- ^ A list of wrapped filter.
  -> p                   -- ^ 'Pandoc' AST node.
  -> p                   -- ^ Transformed node.
applyFilters = applyFilter . fold

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
  => t (PartialFilter p) -- ^ A list of wrapped filter.
  -> (p -> p)            -- ^ Filter function applicable to @p@ directly.
getConcatedFilter = applyFilters

-- | Conversion between monadic filter functions, e.g. from @'Inline' ->
-- 'IO' ['Inline']@ filter to @'Pandoc' -> 'IO' 'Pandoc'@ filter
convertFilterM
  :: (Monad m, ToPartialFilter m f p)
  => f                -- ^ A monadic filter function.
  -> (p -> m p)       -- ^ Monadic filter function acting on @p@.
convertFilterM = applyFilterM . mkFilter

-- | Conversion between filter functions, e.g. from @'Inline' -> ['Inline']@
-- filter to @'Pandoc' -> 'Pandoc'@ filter
convertFilter
  :: (ToPartialFilter Identity f p)
  => f        -- ^ A filter function.
  -> (p -> p) -- ^ Filter function acting on @p@.
convertFilter = applyFilter . mkFilter
