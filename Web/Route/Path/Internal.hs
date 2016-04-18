-- |
-- The internal representation for "Web.Route.Path" types.
-- Most applications should import that module instead.
{-# LANGUAGE FlexibleInstances, GADTs, TypeOperators #-}
module Web.Route.Path.Internal
  ( Path(..)
  , PathRoute(..)
  , maybeA
  ) where

import Control.Applicative (Alternative, empty)
import qualified Control.Invariant.Functor as Inv
import Control.Invariant.Monoidal
import qualified Data.Isomorphism.Type as I
import qualified Data.Isomorphism.Function as I
import Data.String (IsString(..))

import Web.Route.Parameter

-- |A parser/reverse-router isomorphism between URL paths (represented as @[s]@) and a value @a@.
-- The constructors needed by users are:
--
--   * @'PathFixed' s@, which matches a single literal path component.  This can also be created uisng the 'IsString' instance and OverloadedStrings extension.
--   * @'PathParameter'@, which matches a place-holder component for a 'Parameter' type.  This can also be created with an explicit type using 'Web.Route.Path.parameter'.
--
-- Path values can be composed using the 'Monoidal' and 'MonoidalAlt' instances.
data Path s a where
  PathEmpty :: Path s () -- Accepts only the empty path @[]@
  PathFixed :: !s -> Path s ()
  PathParameter :: Parameter s a => Path s a
  PathIso :: (a I.<-> b) -> Path s a -> Path s b
  PathJoin :: Path s a -> Path s b -> Path s (a, b)
  PathChoose :: Path s a -> Path s b -> Path s (Either a b)

instance Inv.Functor (Path s) where
  fmap f (PathIso g p) = PathIso (f I.. g) p
  fmap f p = PathIso f p

instance Monoidal (Path s) where
  unit = PathEmpty
  (>*<) = PathJoin

instance MonoidalAlt (Path s) where
  (>|<) = PathChoose

instance IsString s => IsString (Path s ()) where
  fromString = PathFixed . fromString

-- |A path along with a mapping function, which can allow abstraction over the specific type of the path into a common type.
data PathRoute s a b = PathRoute
  { routePath :: Path s b
  , pathAction :: b -> a
  }

instance Inv.Functor (PathRoute s b) where
  fmap f (PathRoute p g) = PathRoute (Inv.fmap f p) (g . I.isoFrom f)

-- |@maybe 'empty' 'pure'@
maybeA :: Alternative m => Maybe a -> m a
maybeA = maybe empty pure

