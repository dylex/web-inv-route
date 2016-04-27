-- |
{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
module Web.Route.Invertible.Placeholder
  ( Placeholder(..)
  , PlaceholderValue(..)
  , renderPlaceholderValue
  ) where

import Data.String (IsString(..))

import Web.Route.Invertible.String
import Web.Route.Invertible.Parameter

data Placeholder s a where
  PlaceholderFixed :: !s -> Placeholder s ()
  PlaceholderParameter :: Parameter s a => Placeholder s a

instance IsString s => IsString (Placeholder s ()) where
  fromString = PlaceholderFixed . fromString

instance RouteString s => Parameterized s (Placeholder s) where
  parameter = PlaceholderParameter

-- |An concrete, untyped representation of a 'Placeholder' value, distinguishing fixed components from parameters.
data PlaceholderValue s where
  PlaceholderValueFixed :: !s -> PlaceholderValue s
  PlaceholderValueParameter :: Parameter s a => a -> PlaceholderValue s

renderPlaceholderValue :: PlaceholderValue s -> s
renderPlaceholderValue (PlaceholderValueFixed s) = s
renderPlaceholderValue (PlaceholderValueParameter a) = renderParameter a
