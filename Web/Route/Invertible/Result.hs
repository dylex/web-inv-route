-- |Results of route table lookups.
module Web.Route.Invertible.Result
  ( RouteResult(..)
  , routeResult
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Semigroup (Semigroup((<>)))
import Data.Typeable (Typeable)
import Network.HTTP.Types.Header (ResponseHeaders, hAllow)
import Network.HTTP.Types.Status (Status, notFound404, methodNotAllowed405, internalServerError500)

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Method

-- |The result of looking up a request in a routing map.
data RouteResult a
  = RouteNotFound -- ^No route was found to handle this request: 404
  | AllowedMethods [Method] -- ^No route was found to handle this request, but there are routes for other methods: 405
  | RouteResult a -- ^A route was found to handle this request
  | MultipleRoutes -- ^Multiple (conflicting) routes were found to handle this request: 500
  deriving (Eq, Show, Typeable)

instance Functor RouteResult where
  fmap _ RouteNotFound = RouteNotFound
  fmap _ (AllowedMethods m) = AllowedMethods m
  fmap f (RouteResult x) = RouteResult (f x)
  fmap _ MultipleRoutes = MultipleRoutes

instance Semigroup (RouteResult a) where
  RouteNotFound <> r = r
  AllowedMethods _ <> r@(RouteResult _) = r
  AllowedMethods a <> AllowedMethods b = AllowedMethods $ unionSorted a b
  r@(RouteResult _) <> AllowedMethods _ = r
  MultipleRoutes <> _ = MultipleRoutes
  r <> RouteNotFound = r
  _ <> _ = MultipleRoutes

instance Monoid (RouteResult a) where
  mempty = RouteNotFound
  mappend = (<>)

unionSorted :: Ord a => [a] -> [a] -> [a]
unionSorted al@(a:ar) bl@(b:br) = case compare a b of
  LT -> a:unionSorted ar bl
  EQ -> a:unionSorted ar br
  GT -> b:unionSorted al br
unionSorted [] l = l
unionSorted l [] = l

-- |Convert a result to an appropriate HTTP status and headers.
-- It is up to the user to provide an appropriate body (if any).
routeResult :: RouteResult a -> Either (Status, ResponseHeaders) a
routeResult RouteNotFound = Left (notFound404, [])
routeResult (AllowedMethods m) = Left (methodNotAllowed405, [(hAllow, BSC.intercalate (BSC.singleton ',') $ map renderParameter m)])
routeResult (RouteResult a) = Right a
routeResult MultipleRoutes = Left (internalServerError500, [])
