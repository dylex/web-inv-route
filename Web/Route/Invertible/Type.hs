-- |
-- Type-level families and GADTs, mainly around 'Maybe'.
{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}
module Web.Route.Invertible.Type
  ( FromMaybe
  , FromMaybeVoid
  , void
  , TMaybe(..)
  , tmaybe
  , MaybeFunction
  , tmaybeFunction
  , tmaybeApply
  , tmaybeMapFunction
  , When(..)
  , when
  ) where

import Data.Void (Void)

-- |Equivalent to 'Data.Maybe.fromMaybe' as a type level.
type family FromMaybe a (m :: Maybe *) where
  FromMaybe a 'Nothing = a
  FromMaybe a ('Just m) = m

-- |Short-hand for the most common use of 'FromMaybe': @FromMaybe Void@.
type FromMaybeVoid m = FromMaybe Void m

-- |A void placeholder value (i.e., @_|_@).
void :: Void
void = error "Incomplete void route"

-- |A 'Maybe' type lifted over another type constructor.
data TMaybe m (t :: Maybe *) where
  TNothing :: TMaybe m 'Nothing
  TJust :: !(m a) -> TMaybe m ('Just a)

-- |Equivalent to 'maybe' over 'TMaybe'.
tmaybe :: a -> (m (FromMaybeVoid t) -> a) -> TMaybe m t -> a
tmaybe a _ TNothing = a
tmaybe _ f (TJust a) = f a

-- |A function with an optional argument, guarded by a Maybe type.
type family MaybeFunction (a :: Maybe *) b where
  MaybeFunction 'Nothing b = b
  MaybeFunction ('Just a) b = a -> b

-- |Construct a MaybeFunction by inhabiting any void argument.  The first argument is jsut a type proxy.
tmaybeFunction :: TMaybe m a -> (FromMaybeVoid a -> b) -> MaybeFunction a b
tmaybeFunction TNothing f = f void
tmaybeFunction (TJust _) f = f

-- |Apply a MaybeFunction to a FromMaybeVoid.  The first argument is just a type proxy.
tmaybeApply :: TMaybe m a -> MaybeFunction a b -> FromMaybeVoid a -> b
tmaybeApply TNothing f _ = f
tmaybeApply (TJust _) f a = f a

-- |Map over the result of a MaybeFunction.
tmaybeMapFunction :: TMaybe m a -> (b -> c) -> MaybeFunction a b -> MaybeFunction a c
tmaybeMapFunction TNothing f x = f x
tmaybeMapFunction (TJust _) f g = f . g

-- |A value guarded by a type-level Bool, creating a typed Maybe.
data When (t :: Bool) a where
  WhenNot :: When 'False a
  WhenSo :: !a -> When 'True a

instance Functor (When t) where
  fmap _ WhenNot = WhenNot
  fmap f (WhenSo a) = WhenSo (f a)

-- |Equivalent to 'maybe' over 'When'.
when :: b -> (a -> b) -> When t a -> b
when a _ WhenNot = a
when _ f (WhenSo a) = f a

