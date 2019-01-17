-- |
-- The internal representation for sequences of placeholders, such as paths.
-- For example, the following represents a sequence of @['PlaceholderFixed' "item", 'PlaceholderParameter']@:
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Invertible.Monoidal
-- >>> import Web.Route.Invertible.Parameter
-- >>> let p = "item" *< parameter :: Sequence String Int
-- >>> parseSequence p ["item", "123"]
-- 123
-- >>> renderSequence p 123
-- ["item","123"]
--
-- These are used as the basis for path routers and other sequential/nested types.
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, GADTs #-}
module Web.Route.Invertible.Sequence
  ( Sequence(..)
  , placeholderSequence
  , wildcard
  , sequenceValues
  , renderSequence
  , readsSequence
  , parseSequence
  , reverseSequence
  ) where

import           Control.Invertible.Monoidal
import           Control.Invertible.Monoidal.Free
import           Control.Monad (MonadPlus, mzero, guard)
import           Control.Monad.Fail (MonadFail)
import qualified Data.Invertible as I
import           Data.String (IsString(..))

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Placeholder

-- |A parser/reverse-router isomorphism between sequences of strings (represented as @[s]@) and a value @a@.
-- These can be constructed using:
--
--   * @'fromString' s@ (or simply @s@ with OverloadedStrings), which matches a single literal component.
--   * @'parameter'@ (or @'param' (undefined :: T)@ for an explicit type), which matches a place-holder component for a 'Parameter' type.
--
-- Sequence values can then be composed using 'Monoidal' and 'MonoidalAlt'.
newtype Sequence s a = Sequence { freeSequence :: Free (Placeholder s) a }
  deriving (I.Functor, Monoidal, MonoidalAlt)

instance Show s => Show (Sequence s a) where
  showsPrec d (Sequence s) = showParen (d > 10) $
    showString "Sequence " . showsFree (showsPrec 11) s

-- |Make a singleton 'Sequence' out of a 'Placeholder'.
placeholderSequence :: Placeholder s a -> Sequence s a
placeholderSequence = Sequence . Free

instance Parameterized s (Sequence s) where
  parameter = placeholderSequence parameter

instance IsString s => IsString (Sequence s ()) where
  fromString = placeholderSequence . fromString

-- |Ignore an arbitrary sequence of parameters (usually as a tail), always generating the same thing.
wildcard :: (Parameterized s f, MonoidalAlt f, Parameter s a) => [a] -> f ()
wildcard d = d >$ manyI parameter

-- |Realize a 'Sequence' as instantiated by a value to a sequence of 'PlaceholderValue's.
sequenceValues :: Sequence s a -> a -> [PlaceholderValue s]
sequenceValues = produceFree f . freeSequence where
  f :: Placeholder s a' -> a' -> PlaceholderValue s
  f (PlaceholderFixed t) () = PlaceholderValueFixed t
  f PlaceholderParameter a = PlaceholderValueParameter a

-- |Render a 'Sequence' as instantiated by a value to a list of string segments.
renderSequence :: Sequence s a -> a -> [s]
renderSequence p = map renderPlaceholderValue . sequenceValues p

-- |Attempt to parse sequence segments into a value and remaining (unparsed) segments, ala 'reads'.
readsSequence :: forall m s a . (MonadPlus m, Eq s) => Sequence s a -> [s] -> m (a, [s])
readsSequence = parseFree f . freeSequence where
  f :: Placeholder s a' -> s -> m a'
  f (PlaceholderFixed t) a = guard (a == t)
  f PlaceholderParameter a = maybe mzero return (parseParameter a)

-- |Parse a sequence into possible values.  Can return all possible values as a list or (usually) a single value as 'Maybe'.
parseSequence :: (MonadPlus m, MonadFail m, Eq s) => Sequence s a -> [s] -> m a
parseSequence p l = do
  (a, []) <- readsSequence p l
  return a

-- |Reverse the order of a sequence, such that @reverseSequence p@ parses/produces @reverse l@ iff @p@ parses/produces @l@.
-- Since sequences are matched left-to-right, this lets you match them right-to-left.
-- It probably goes without saying, but this won't work for infinite sequences, such as those produced by 'while'.
reverseSequence :: Sequence s a -> Sequence s a
reverseSequence = Sequence . reverseFree . freeSequence
