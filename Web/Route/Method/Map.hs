module Web.Route.Method.Map
  ( MethodMap
  , module Web.Route.Map
  , fallbackHEADtoGET
  , lookupMethod
  ) where

import qualified Data.Map.Strict as M
import Network.HTTP.Types.Method (StdMethod(GET, HEAD), Method)

import Web.Route.Parameter
import Web.Route.Map

type MethodMap a = MonoidMap StdMethod a

fallbackHEADtoGET :: MethodMap a -> MethodMap a
fallbackHEADtoGET = fallback HEAD GET

lookupMethod :: Method -> MethodMap a -> Either [Method] a
lookupMethod s (MonoidMap m) =
  maybe (Left $ map renderParameter $ M.keys m) Right $ lookupParameter' s $ MonoidMap m
