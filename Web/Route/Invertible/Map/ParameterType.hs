-- |
-- A dynamic parameter-parsing map.
module Web.Route.Invertible.Map.ParameterType
  ( ParameterTypeMap
  , singleton
  , insert
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.Dynamic (Dynamic, toDyn)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)

import Web.Route.Invertible.Parameter
import qualified Web.Route.Invertible.Map.Monoid as MM

-- |A map of 'ParameterType's.
type ParameterTypeMap s a = MM.MonoidMap (ParameterType s) a

-- |Equivalent to @'MM.singleton' . 'parameterTypeOf'@
singleton :: Parameter s p => proxy p -> a -> ParameterTypeMap s a
singleton p = MM.MonoidMap . M.singleton (parameterTypeOf p)

-- |Equivalent to @'MM.insert' . 'parameterTypeOf'@
insert :: Parameter s p => proxy p -> a -> ParameterTypeMap s a -> ParameterTypeMap s a
insert p a (MM.MonoidMap m) = MM.MonoidMap $ M.insert (parameterTypeOf p) a m

-- |/O(n)/. Find all types in the map that can parse the given string data, returning 'toDyn' of the parsed string and the associated map value.
lookup :: s -> ParameterTypeMap s a -> [(Dynamic, a)]
lookup s (MM.MonoidMap m) = do
  (ParameterType t, nt) <- M.toList m
  x <- maybeToList $ parseParameterAs t s
  return (toDyn x, nt)
