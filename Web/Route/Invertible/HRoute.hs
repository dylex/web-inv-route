{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators #-}
module Web.Route.Invertible.HRoute
  ( Route(..)
  , Action(..)
  , mapRouteFunctions
  , requestRoute'
  , requestRoute
  ) where

import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path
import Web.Route.Invertible.Request
import Web.Route.Invertible.Type

data Route (l :: [*]) where
  Route :: Route '[]
  RouteHost     :: { unRouteHost :: !(Route l), routeHost     :: !(Host h) } -> Route (h ': l)
  RouteSecure   :: { unRoute     :: !(Route l), routeSecure   :: !Bool     } -> Route l
  RoutePath     :: { unRoutePath :: !(Route l), routePath     :: !(Path p) } -> Route (p ': l)
  RouteMethod   :: { unRoute     :: !(Route l), routeMethod   :: !Method   } -> Route l
  RoutePriority :: { unRoute     :: !(Route l), routePriority :: !Int      } -> Route l

data Action l a = Action
  { actionRoute :: !(Route l)
  , routeAction :: !(FunctionsRev l a)
  }

infixl 0 `RouteHost`, `RouteSecure`, `RoutePath`, `RouteMethod`, `RoutePriority`, `Action`

mapRouteFunctions :: Route l -> (a -> b) -> Functions l a -> Functions l b
mapRouteFunctions Route           f g = f g
mapRouteFunctions (RouteHost r _) f g = \h -> mapRouteFunctions r f (g h)
mapRouteFunctions (RoutePath r _) f g = \p -> mapRouteFunctions r f (g p)
mapRouteFunctions r               f g = mapRouteFunctions (unRoute r) f g

requestRoute' :: Route l -> Request -> Functions l Request
requestRoute' Route q = q
requestRoute' (RouteHost r (HostRev s)) q = \h -> requestRoute' r q{ requestHost = renderSequence s h }
requestRoute' (RouteSecure r s)         q =       requestRoute' r q{ requestSecure = s }
requestRoute' (RoutePath r (Path s))    q = \p -> requestRoute' r q{ requestPath = renderSequence s p }
requestRoute' (RouteMethod r m)         q =       requestRoute' r q{ requestMethod = m }
requestRoute' (RoutePriority r _)       q =       requestRoute' r q

requestRoute :: Route l -> Functions l Request
requestRoute r = requestRoute' r blankRequest
