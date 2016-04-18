{-# LANGUAGE TypeFamilies #-}
module Web.Route.Class
  ( Route(..)
  ) where

import Network.HTTP.Types.Header (ResponseHeaders, hAllow)
import Network.HTTP.Types.Status (Status, notFound404, methodNotAllowed405)

import Web.Route.String
import Web.Route.Request
import Web.Route.Resolver

class Route (r :: * -> * -> * -> *) where
  type RouteMap r s a :: *
  route :: RouteString s => r s a b -> RouteMap r s a
  routeRequest :: RouteString s => Request s -> RouteMap r s a -> Either (Status, ResponseHeaders) a

newtype IdentityRoute s a b = IdentityRoute a

instance Route IdentityRoute where
  type RouteMap IdentityRoute s a = Exactly a
  route (IdentityRoute a) = Exactly a
  routeRequest _ Blank = Left (notFound404, [])
  routeRequest _ (Exactly a) = Right a
  routeRequest q Conflict = error $ "conflict routing " ++ show q

newtype PrioritizedRoute r s a b = PrioritizedRoute (Prioritized (r s a b))

instance Route r => Route (PrioritizedRoute r) where
  type RouteMap (PrioritizedRoute r) s a = Prioritized (RouteMap r s a)
