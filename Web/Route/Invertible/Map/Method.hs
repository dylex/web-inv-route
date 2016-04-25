module Web.Route.Invertible.Map.Method
  ( MethodMap
  , fallbackHEADtoGET
  , lookupMethod
  ) where

import qualified Data.Map.Strict as M
import Network.HTTP.Types.Method (StdMethod(GET, HEAD), Method)

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Map
import Web.Route.Invertible.Map.Monoid

type MethodMap a = MonoidMap StdMethod a

fallbackHEADtoGET :: MethodMap a -> MethodMap a
fallbackHEADtoGET (MonoidMap m) = MonoidMap $ fallback HEAD GET m

lookupMethod :: Method -> MethodMap a -> Either [Method] a
lookupMethod s (MonoidMap m) =
  maybe (Left $ map renderParameter $ M.keys m) Right $ lookupParameter s m
