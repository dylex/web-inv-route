{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, ExistentialQuantification #-}
module Web.Route.Invertible.HRoute
  ( Route(..)
  , Action(..)
  , mapRouteFunctions
  , requestRoute'
  , requestRoute
  , SomeAction(..)
  ) where

import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path
import Web.Route.Invertible.Request
import Web.Route.Invertible.Type

data Route (l :: [*]) where
  Route :: Route '[]
  RouteHost     :: { routeHost     :: !(Host h), unRouteTail :: !(Route l) } -> Route (h ': l)
  RouteSecure   :: { routeSecure   :: !Bool    , unRoute     :: !(Route l) } -> Route l
  RoutePath     :: { routePath     :: !(Path p), unRouteTail :: !(Route l) } -> Route (p ': l)
  RouteMethod   :: { routeMethod   :: !Method  , unRoute     :: !(Route l) } -> Route l
  RoutePriority :: { routePriority :: !Int     , unRoute     :: !(Route l) } -> Route l

data Action l a = Action
  { actionRoute :: !(Route l)
  , routeAction :: !(Functions l a)
  }

infixr 1 `RouteHost`, `RouteSecure`, `RoutePath`, `RouteMethod`, `RoutePriority`
infix 0 `Action`

mapRouteFunctions :: Route l -> (a -> b) -> Functions l a -> Functions l b
mapRouteFunctions Route           f g = f g
mapRouteFunctions (RouteHost _ r) f g = \h -> mapRouteFunctions r f (g h)
mapRouteFunctions (RoutePath _ r) f g = \p -> mapRouteFunctions r f (g p)
mapRouteFunctions r               f g = mapRouteFunctions (unRoute r) f g

requestRoute' :: Route l -> Request -> Functions l Request
requestRoute' Route q = q
requestRoute' (RouteHost (HostRev s) r) q = \h -> requestRoute' r q{ requestHost = renderSequence s h }
requestRoute' (RouteSecure s         r) q =       requestRoute' r q{ requestSecure = s }
requestRoute' (RoutePath (Path s)    r) q = \p -> requestRoute' r q{ requestPath = renderSequence s p }
requestRoute' (RouteMethod m         r) q =       requestRoute' r q{ requestMethod = m }
requestRoute' (RoutePriority _       r) q =       requestRoute' r q

requestRoute :: Route l -> Functions l Request
requestRoute r = requestRoute' r blankRequest

data SomeAction a = forall l . SomeAction (Action l a)

normalizeAction :: Action l a -> SomeAction a
normalizeAction (Action Route a) = SomeAction $ Action Route a
normalizeAction (Action (RoutePath p (RouteHost h r)) a) = SomeAction $ Action (RouteHost h (RoutePath p r)) $ flip a
