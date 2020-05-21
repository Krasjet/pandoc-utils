{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
  ToPartialFilter (..)
  ) where

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
