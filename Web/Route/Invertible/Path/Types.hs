-- |
-- The internal representation for "Web.Route.Invertible.Path" types.
-- Most applications should import that module instead.
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GADTs, TypeOperators #-}
module Web.Route.Invertible.Path.Types
  ( PathString
  , Path(..)
  , parameter
  ) where

import Control.Invertible.Monoidal
import qualified Data.Invertible as I
import Data.Text (Text)
import Data.String (IsString(..))

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Placeholder

type PathString = Text

-- |A parser/reverse-router isomorphism between URL paths (represented as @[PathString]@) and a value @a@.
-- These can be constructed using:
--
--   * @'fromString' s@ (or simply @s@ with OverloadedStrings), which matches a single literal path component.
--   * @'parameter'@ (or @'param' (undefined :: T)@ for an explicit type), which matches a place-holder component for a 'Parameter' type.
--
-- Path values can then be composed using 'Monoidal' and 'MonoidalAlt'.
data Path a where
  PathEmpty :: Path () -- Accepts only the empty path @[]@
  PathPlaceholder :: !(Placeholder PathString a) -> Path a
  PathTransform :: !(a I.<-> b) -> Path a -> Path b
  PathJoin :: Path a -> Path b -> Path (a, b)
  PathChoose :: Path a -> Path b -> Path (Either a b)

instance I.Functor Path where
  fmap f (PathTransform g p) = PathTransform (f I.. g) p
  fmap f p = PathTransform f p

instance Monoidal Path where
  unit = PathEmpty
  (>*<) = PathJoin

instance MonoidalAlt Path where
  (>|<) = PathChoose

instance Parameterized PathString Path where
  parameter = PathPlaceholder parameter

instance IsString (Path ()) where
  fromString = PathPlaceholder . fromString
