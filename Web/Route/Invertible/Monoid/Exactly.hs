-- |
-- A monoid that only admits a single value.
{-# LANGUAGE CPP #-}
module Web.Route.Invertible.Monoid.Exactly
  ( Exactly(..)
  , maybeToExactly
  , exactlyToMaybe
  , listToExactly
  , exactlyToList
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))

-- |A 'Maybe'-like monoid that only allows one value, overflowing into 'Conflict' when more than one 'Exactly' are combined (with '<|>' or 'Data.Monoid.<>', which thus function identically).
data Exactly a
  = Blank -- ^ 'Nothing'
  | Exactly a -- ^ 'Just'
  | Conflict -- ^ 'fail': 'error' in most cases that attempt to access a value
  deriving (Eq, Ord, Show, Read)

instance Functor Exactly where
  fmap _ Blank = Blank
  fmap f (Exactly a) = Exactly (f a)
  fmap _ Conflict = Conflict
-- |Conflict always overrides other values.
instance Applicative Exactly where
  pure = Exactly
  Exactly f <*> Exactly x = Exactly (f x)
  _ <*> Conflict = Conflict
  Conflict <*> _ = Conflict
  _ <*> _ = Blank
-- |Same as for 'Maybe', except that @Exactly _ <|> Exactly _ = Conflict@.
instance Alternative Exactly where
  empty = Blank
  Blank <|> e = e
  e <|> Blank = e
  _ <|> _ = Conflict  
instance Monad Exactly where
  Blank >>= _ = Blank
  Exactly x >>= f = f x
  Conflict >>= _ = Conflict
  Exactly _ >> e = e
  Conflict >> _ = Conflict
  _ >> Conflict = Conflict
  _ >> _ = Blank
#if MIN_VERSION_base(4,13,0)
instance MonadFail Exactly where
#endif
  fail _ = Conflict
instance MonadPlus Exactly

instance Semigroup (Exactly a) where
  (<>) = (<|>)

-- |Combines using the 'Alternative' instance, similar to an @'Data.Monoid.Alt' 'Maybe'@.
instance Monoid (Exactly a) where
  mempty = Blank
  mappend = (<|>)

instance Foldable Exactly where
  foldr _ z Blank = z
  foldr f z (Exactly x) = f x z
  foldr f z Conflict = f (error "foldr: Conflict") z
  foldl _ z Blank = z
  foldl f z (Exactly x) = f z x 
  foldl f z Conflict = f z (error "foldl: Conflict")
instance Traversable Exactly where
  traverse _ Blank = pure Blank
  traverse f (Exactly x) = Exactly <$> f x 
  traverse _ Conflict = pure Conflict

-- |@exactlyToMaybe . maybeToExactly == id@
maybeToExactly :: Maybe a -> Exactly a
maybeToExactly Nothing = Blank
maybeToExactly (Just x) = Exactly x

-- |@exactlyToMaybe Conflict@ is an error.
exactlyToMaybe :: Exactly a -> Maybe a
exactlyToMaybe Blank = Nothing
exactlyToMaybe (Exactly x) = Just x
exactlyToMaybe Conflict = error "exactlyToMaybe: Conflict"

-- |Conflict for any list with more than one element.
listToExactly :: [a] -> Exactly a
listToExactly [] = Blank
listToExactly [x] = Exactly x
listToExactly _ = Conflict

-- |@exactlyToList Conflict@ is an error.
exactlyToList :: Exactly a -> [a]
exactlyToList Blank = []
exactlyToList (Exactly x) = [x]
exactlyToList Conflict = error "exactlyToList: Conflict"
