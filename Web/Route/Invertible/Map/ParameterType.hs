-- |
-- A dynamic parameter-parsing map.
module Web.Route.Invertible.Map.ParameterType
  ( ParameterTypeMap
  , singletonParameterType
  , insertParameterType
  , lookupParameterType
  ) where

import Prelude hiding (lookup)

import Data.Dynamic (Dynamic, toDyn)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Map.Monoid

-- |A map of 'ParameterType's.
type ParameterTypeMap s a = MonoidMap (ParameterType s) a

-- |Equivalent to @'MM.singleton' . 'parameterTypeOf'@
singletonParameterType :: Parameter s p => proxy p -> a -> ParameterTypeMap s a
singletonParameterType p = MonoidMap . M.singleton (parameterTypeOf p)

-- |Equivalent to @'MM.insert' . 'parameterTypeOf'@
insertParameterType :: Parameter s p => proxy p -> a -> ParameterTypeMap s a -> ParameterTypeMap s a
insertParameterType p a (MonoidMap m) = MonoidMap $ M.insert (parameterTypeOf p) a m

-- |/O(n)/. Find all types in the map that can parse the given string data, returning 'toDyn' of the parsed string and the associated map value.
lookupParameterType :: s -> ParameterTypeMap s a -> [(Dynamic, a)]
lookupParameterType s (MonoidMap m) = do
  (ParameterType t, nt) <- M.toList m
  x <- maybeToList $ parseParameterAs t s
  return (toDyn x, nt)
