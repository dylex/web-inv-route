-- |
module Web.Route.Invertible.Map
  ( lookupParameter
  , reduce
  , lookupExactly
  , fallback
  ) where

import Prelude hiding (lookup)

import Control.Monad ((<=<))
import qualified Data.Map.Strict as M

import Web.Route.Invertible.Monoid.Exactly
import Web.Route.Invertible.Parameter

-- |Combine 'parseParameter' and 'M.lookup'.
lookupParameter :: (Ord p, Parameter s p) => s -> M.Map p a -> Maybe a
lookupParameter s m = parseParameter s >>= (`M.lookup` m)

-- |Eliminate 'Blank' values, and (strictly) produce an error for 'Conflict' values.
reduce :: M.Map k (Exactly a) -> M.Map k a
reduce = M.mapMaybe exactlyToMaybe

-- |Combine 'exactlyToMaybe' and 'M.lookup'.
lookupExactly :: Ord k => k -> M.Map k (Exactly a) -> Maybe a
lookupExactly k = exactlyToMaybe <=< M.lookup k

-- |When the first key is not in the map, produce a new map that falls back to the second key instead.
-- That is, create a new map where the second key's value is copied to the first key without overwriting.
fallback :: Ord k => k -> k -> M.Map k a -> M.Map k a
fallback from to m
  | Just v <- M.lookup to m = M.insertWith (\_ -> id) from v m
  | otherwise = m
