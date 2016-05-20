-- |
-- Functionality specific to 'MonoidMap's over 'Method's.
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

-- |A 'MonoidMap' keyed on 'Method'
type MethodMap = MonoidMap Method

-- |If there is no value associated with 'HEAD', 'fallback' to the value associated with 'GET'.
fallbackMethodHEADtoGET :: MethodMap a -> MethodMap a
fallbackMethodHEADtoGET = MonoidMap . fallback HEAD GET . monoidMap

-- |'fallbackMethodHEADtoGET' over 'DefaultMap'.
fallbackDefaultMethodHEADtoGET :: DefaultMap MethodMap a -> DefaultMap MethodMap a
fallbackDefaultMethodHEADtoGET = withDefaultMap fallbackMethodHEADtoGET

-- |Either the given value or the list of keys.
orKeys :: MethodMap a -> Maybe a -> Either [Method] a
orKeys (MonoidMap m) = maybe (Left $ M.keys m) Right

-- |Lookup a method in the map, returning either the associated value, or the list of keys.
-- This is useful for generating 405 results.
lookupMethod :: Method -> MethodMap a -> Either [Method] a
lookupMethod s m =
  orKeys m $ M.lookup s $ monoidMap m

-- |'lookupMethod' over 'DefaultMap'.
lookupDefaultMethod :: Method -> DefaultMap MethodMap a -> Either [Method] a
lookupDefaultMethod s d =
  orKeys (defaultMap d) $ lookupDefault (M.lookup s . monoidMap) d
