-- |Allow more general "Web.Route.Invertible.Parameter" placeholders that include fixed strings.
{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
module Web.Route.Invertible.Placeholder
  ( Placeholder(..)
  , renderPlaceholder
  , PlaceholderValue(..)
  , renderPlaceholderValue
  ) where

import Data.Function (on)
import Data.String (IsString(..))
import Data.Typeable (typeRep, typeOf)

import Web.Route.Invertible.String
import Web.Route.Invertible.Parameter

-- |A segment of a parser over strings @s@, which may be a fixed string (usually created through 'IsString'), only accepting a single fixed value, or a dynamic parameter (created through 'Parameterized'), which encapsulates a 'Parameter' type.
data Placeholder s a where
  PlaceholderFixed :: !s -> Placeholder s ()
  PlaceholderParameter :: Parameter s a => Placeholder s a

instance Eq s => Eq (Placeholder s a) where
  PlaceholderFixed x == PlaceholderFixed y = x == y
  PlaceholderParameter == PlaceholderParameter = True
  _ == _ = False

instance Ord s => Ord (Placeholder s a) where
  PlaceholderFixed x `compare` PlaceholderFixed y = x `compare` y
  PlaceholderFixed _ `compare` PlaceholderParameter = LT
  PlaceholderParameter `compare` PlaceholderFixed _ = GT
  PlaceholderParameter `compare` PlaceholderParameter = EQ

instance Show s => Show (Placeholder s a) where
  showsPrec d (PlaceholderFixed s) = showParen (d > 10) $
    showString "PlaceholderFixed " . showsPrec 11 s
  showsPrec d p@PlaceholderParameter = showParen (d > 10) $
    showString "PlaceholderParameter " . showsPrec 11 (typeRep p)

instance IsString s => IsString (Placeholder s ()) where
  fromString = PlaceholderFixed . fromString

instance RouteString s => Parameterized s (Placeholder s) where
  parameter = PlaceholderParameter

-- |Render a placeholder into a string, as fixed text or using 'renderParameter'.
renderPlaceholder :: Placeholder s a -> a -> s
renderPlaceholder (PlaceholderFixed s) () = s
renderPlaceholder PlaceholderParameter a = renderParameter a

-- |A concrete, untyped representation of a parsed 'Placeholder' value, distinguishing fixed components from parameters but abstracting over the parsed type.
data PlaceholderValue s where
  PlaceholderValueFixed :: !s -> PlaceholderValue s
  PlaceholderValueParameter :: Parameter s a => a -> PlaceholderValue s

instance Eq s => Eq (PlaceholderValue s) where
  (==) = on (==) renderPlaceholderValue

instance Ord s => Ord (PlaceholderValue s) where
  compare = on compare renderPlaceholderValue

instance (RouteString s, Show s) => Show (PlaceholderValue s) where
  showsPrec d (PlaceholderValueFixed s) = showParen (d > 10) $
    showString "PlaceholderValueFixed " . showsPrec 11 s
  showsPrec d p@(PlaceholderValueParameter a) = showParen (d > 10) $
    showString "PlaceholderValueParameter (" .
    showString (toString $ renderPlaceholderValue p) .
    showString " :: " .
    shows (typeOf a) .
    showChar ')'

-- |Render a placeholder into a string, as fixed text or using 'renderParameter'.
renderPlaceholderValue :: PlaceholderValue s -> s
renderPlaceholderValue (PlaceholderValueFixed s) = s
renderPlaceholderValue (PlaceholderValueParameter a) = renderParameter a
