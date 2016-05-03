{-# LANGUAGE GADTs, DataKinds, TypeFamilies, RecordWildCards, MultiParamTypeClasses #-}
module Web.Route.Invertible.Type
  ( FromMaybe
  , FromMaybeUnit
  , TMaybe(..)
  , tmaybe
  , TWhen(..)
  , tIf
  ) where

type family FromMaybe a (m :: Maybe *) where
  FromMaybe a 'Nothing = a
  FromMaybe a ('Just m) = m

type FromMaybeUnit m = FromMaybe () m

data TMaybe m (t :: Maybe *) where
  TNothing :: TMaybe m 'Nothing
  TJust :: !(m a) -> TMaybe m ('Just a)

tmaybe :: a -> (m (FromMaybeUnit t) -> a) -> MatchMaybe m t -> a
tmaybe a _ MatchNothing = a
tmaybe _ f (MatchJust a) = f a

data TWhen m (t :: Bool) where
  TFalse :: MatchIf a 'False
  TTrue :: !a -> MatchIf a 'True

tIf :: TWhen a t -> Maybe a
tIf TFalse = Nothing
tIf (TTrue a) = Just a

