module Web.Route.Invertible.Map.Method
  ( MethodMap
  , fallbackHEADtoGET
  , lookup
  ) where

import Prelude hiding (lookup)

import qualified Data.Map.Strict as M

import Web.Route.Invertible.Method
import Web.Route.Invertible.Map
import qualified Web.Route.Invertible.Map.Monoid as MM

type MethodMap a = MM.MonoidMap Method a

fallbackHEADtoGET :: MethodMap a -> MethodMap a
fallbackHEADtoGET (MM.MonoidMap m) = MM.MonoidMap $ fallback HEAD GET m

lookup :: Method -> MethodMap a -> Either [Method] a
lookup s (MM.MonoidMap m) =
  maybe (Left $ M.keys m) Right $ M.lookup s m
