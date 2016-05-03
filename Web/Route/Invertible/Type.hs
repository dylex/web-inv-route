{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}
module Web.Route.Invertible.Type
  ( FromMaybe
  , FromMaybeUnit
  , TMaybe(..)
  , tmaybe
  , When(..)
  , when
  ) where

type family FromMaybe a (m :: Maybe *) where
  FromMaybe a 'Nothing = a
  FromMaybe a ('Just m) = m

type FromMaybeUnit m = FromMaybe () m

data TMaybe m (t :: Maybe *) where
  TNothing :: TMaybe m 'Nothing
  TJust :: !(m a) -> TMaybe m ('Just a)

tmaybe :: a -> (m (FromMaybeUnit t) -> a) -> TMaybe m t -> a
tmaybe a _ TNothing = a
tmaybe _ f (TJust a) = f a

data When (t :: Bool) a where
  WhenNot :: When 'False a
  WhenSo :: !a -> When 'True a

instance Functor (When t) where
  fmap _ WhenNot = WhenNot
  fmap f (WhenSo a) = WhenSo (f a)

when :: b -> (a -> b) -> When t a -> b
when a _ WhenNot = a
when _ f (WhenSo a) = f a

