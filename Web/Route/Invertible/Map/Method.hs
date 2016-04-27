module Web.Route.Invertible.Map.Method
  ( MethodMap
  , fallbackMethodHEADtoGET
  , fallbackDefaultMethodHEADtoGET
  , lookupMethod
  , lookupDefaultMethod
  ) where

import Prelude hiding (lookup)

import qualified Data.Map.Strict as M

import Web.Route.Invertible.Method
import Web.Route.Invertible.Map
import Web.Route.Invertible.Map.Monoid
import Web.Route.Invertible.Map.Default

type MethodMap = MonoidMap Method

fallbackMethodHEADtoGET :: MethodMap a -> MethodMap a
fallbackMethodHEADtoGET = MonoidMap . fallback HEAD GET . monoidMap

fallbackDefaultMethodHEADtoGET :: DefaultMap MethodMap a -> DefaultMap MethodMap a
fallbackDefaultMethodHEADtoGET (DefaultMap m d) = DefaultMap (fallbackMethodHEADtoGET m) d

orKeys :: MethodMap a -> Maybe a -> Either [Method] a
orKeys (MonoidMap m) = maybe (Left $ M.keys m) Right

lookupMethod :: Method -> MethodMap a -> Either [Method] a
lookupMethod s m =
  orKeys m $ M.lookup s $ monoidMap m

lookupDefaultMethod :: Method -> DefaultMap MethodMap a -> Either [Method] a
lookupDefaultMethod s d =
  orKeys (defaultMap d) $ lookupDefault (M.lookup s . monoidMap) d
