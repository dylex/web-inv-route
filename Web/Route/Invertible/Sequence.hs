-- |
-- The internal representation for sequences of placeholders, such as paths.
-- For example, the following represents a sequence of @['PlaceholderFixed' "item", 'PlaceholderParameter']@:
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Invertible.Monoidal
-- >>> let p = "item" *< parameter :: Sequence String Int
-- >>> parseSequence p $ map T.pack ["item", "123"]
-- 123
-- >>> renderSequence p 123
-- ["item","123"]
--
-- These are used as the basis for path routers and other sequential/nested types.
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GADTs, TypeOperators, TupleSections #-}
module Web.Route.Invertible.Sequence
  ( Sequence(..)
  , wildcard
  , sequenceValues
  , renderSequence
  , readsSequence
  , parseSequence
  , reverseSequence
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Invertible.Monoidal
import Control.Monad (MonadPlus, mzero, guard)
import qualified Data.Invertible as I
import Data.String (IsString(..))

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Placeholder

-- |A parser/reverse-router isomorphism between sequences of strings (represented as @[s]@) and a value @a@.
-- These can be constructed using:
--
--   * @'fromString' s@ (or simply @s@ with OverloadedStrings), which matches a single literal component.
--   * @'parameter'@ (or @'param' (undefined :: T)@ for an explicit type), which matches a place-holder component for a 'Parameter' type.
--
-- Sequence values can then be composed using 'Monoidal' and 'MonoidalAlt'.
data Sequence s a where
  SequenceEmpty :: Sequence s () -- Accepts only the empty list @[]@
  SequencePlaceholder :: !(Placeholder s a) -> Sequence s a
  SequenceTransform :: !(a I.<-> b) -> Sequence s a -> Sequence s b
  SequenceJoin :: Sequence s a -> Sequence s b -> Sequence s (a, b)
  SequenceChoose :: Sequence s a -> Sequence s b -> Sequence s (Either a b)

instance I.Functor (Sequence s) where
  fmap f (SequenceTransform g p) = SequenceTransform (f I.. g) p
  fmap f p = SequenceTransform f p

instance Monoidal (Sequence s) where
  unit = SequenceEmpty
  (>*<) = SequenceJoin

instance MonoidalAlt (Sequence s) where
  (>|<) = SequenceChoose

instance Parameterized s (Sequence s) where
  parameter = SequencePlaceholder parameter

instance IsString s => IsString (Sequence s ()) where
  fromString = SequencePlaceholder . fromString

-- |Ignore an arbitrary sequence tail of parameters, always generating the same thing.
wildcard :: (Parameterized s f, MonoidalAlt f, Parameter s a) => [a] -> f ()
wildcard d = d >$ manyI parameter

-- |Realize a 'Sequence' as instantiated by a value to a sequence of 'PlaceholderValue's.
sequenceValues :: Sequence s a -> a -> [PlaceholderValue s]
sequenceValues SequenceEmpty () = []
sequenceValues (SequencePlaceholder (PlaceholderFixed t)) () = [PlaceholderValueFixed t]
sequenceValues (SequencePlaceholder (PlaceholderParameter)) a = [PlaceholderValueParameter a]
sequenceValues (SequenceTransform f p) a = sequenceValues p $ I.biFrom f a
sequenceValues (SequenceJoin p q) (a, b) = sequenceValues p a ++ sequenceValues q b
sequenceValues (SequenceChoose p _) (Left a) = sequenceValues p a
sequenceValues (SequenceChoose _ p) (Right a) = sequenceValues p a

-- |Render a 'Sequence' as instantiated by a value to a list of string segments.
renderSequence :: Sequence s a -> a -> [s]
renderSequence p = map renderPlaceholderValue . sequenceValues p

-- |Attempt to parse sequence segments into a value and remaining (unparsed) segments, ala 'reads'.
readsSequence :: (MonadPlus m, Eq s) => Sequence s a -> [s] -> m (a, [s])
readsSequence SequenceEmpty l = return ((), l)
readsSequence (SequencePlaceholder (PlaceholderFixed t)) (a:l) = (, l) <$> guard (a == t)
readsSequence (SequencePlaceholder (PlaceholderParameter)) (a:l) = (, l) <$> maybe mzero return (parseParameter a)
readsSequence (SequenceTransform f p) a = first (I.biTo f) <$> readsSequence p a
readsSequence (SequenceJoin p q) a = do
  (pr, a') <- readsSequence p a
  first (pr, ) <$> readsSequence q a'
readsSequence (SequenceChoose p q) a = first Left <$> readsSequence p a <|> first Right <$> readsSequence q a
readsSequence _ [] = mzero

-- |Parse a sequence into possible values.  Can return all possible values as a list or (usually) a single value as 'Maybe'.
parseSequence :: (MonadPlus m, Eq s) => Sequence s a -> [s] -> m a
parseSequence p l = do
  (a, []) <- readsSequence p l
  return a

-- |Reverse the order of a sequence, such that @reverseSequence p@ parses/produces @reverse l@ iff @p@ parses/produces @l@.
-- Since sequences are matched left-to-right, this lets you match them right-to-left.
-- It probably goes without saying, but this won't work for infinite sequences, such as those produced by 'while'.
reverseSequence :: Sequence s a -> Sequence s a
reverseSequence (SequenceTransform f (SequenceJoin p q)) = SequenceTransform (f I.. I.swap) (SequenceJoin (reverseSequence q) (reverseSequence p))
reverseSequence (SequenceTransform f p) = SequenceTransform f (reverseSequence p)
reverseSequence (SequenceJoin p q) = SequenceTransform I.swap $ SequenceJoin (reverseSequence q) (reverseSequence p)
reverseSequence (SequenceChoose p q) = SequenceChoose (reverseSequence p) (reverseSequence q)
reverseSequence p = p
