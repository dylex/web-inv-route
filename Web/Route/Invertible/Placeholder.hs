-- |Allow more general "Web.Route.Invertible.Parameter" placeholders that include fixed strings.
{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
module Web.Route.Invertible.Placeholder
  ( Placeholder(..)
  , PlaceholderValue(..)
  , renderPlaceholderValue
  ) where

import Data.String (IsString(..))

import Web.Route.Invertible.String
import Web.Route.Invertible.Parameter

-- |A segment of a parser over strings @s@, which may be a fixed 'PlaceholderFixed' string, only accepting a single fixed value, or a dynamic 'PlaceholderParameter', which encapsulates a 'Parameter' type.
data Placeholder s a where
  PlaceholderFixed :: !s -> Placeholder s ()
  PlaceholderParameter :: Parameter s a => Placeholder s a

instance IsString s => IsString (Placeholder s ()) where
  fromString = PlaceholderFixed . fromString

instance RouteString s => Parameterized s (Placeholder s) where
  parameter = PlaceholderParameter

-- |A concrete, untyped representation of a parsed 'Placeholder' value, distinguishing fixed components from parameters but abstracting over the parsed type.
data PlaceholderValue s where
  PlaceholderValueFixed :: !s -> PlaceholderValue s
  PlaceholderValueParameter :: Parameter s a => a -> PlaceholderValue s

-- |Render a placeholder into a string, as fixed text or using 'renderParameter'.
renderPlaceholderValue :: PlaceholderValue s -> s
renderPlaceholderValue (PlaceholderValueFixed s) = s
renderPlaceholderValue (PlaceholderValueParameter a) = renderParameter a
