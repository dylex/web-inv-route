-- |
-- A dynamic parameter-parsing map.
module Web.Route.Parameter.TypeMap
  ( ParameterTypeMap
  , R.empty
  , singleton
  , insert
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.Dynamic (Dynamic, toDyn)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)

import Web.Route.Parameter
import qualified Web.Route.Map as R

-- |A map of 'ParameterType's.
type ParameterTypeMap s a = R.MonoidMap (ParameterType s) a

-- |Equivalent to @'R.singleton' . 'parameterTypeOf'@
singleton :: Parameter s p => proxy p -> a -> ParameterTypeMap s a
singleton = R.singleton . parameterTypeOf

-- |Equivalent to @'R.insert' . 'parameterTypeOf'@
insert :: Parameter s p => proxy p -> a -> ParameterTypeMap s a -> ParameterTypeMap s a
insert p a (R.MonoidMap m) = R.MonoidMap $ M.insert (parameterTypeOf p) a m

-- |/O(n)/. Find all types in the map that can parse the given string data, returning 'toDyn' of the parsed string and the associated map value.
lookup :: s -> ParameterTypeMap s a -> [(Dynamic, a)]
lookup s (R.MonoidMap m) = do
  (ParameterType t, nt) <- M.toList m
  x <- maybeToList $ parseParameterAs t s
  return (toDyn x, nt)
