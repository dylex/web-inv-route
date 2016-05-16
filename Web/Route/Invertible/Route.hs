-- |Single-route construction.
-- This package lets you describe the individual end-points for routing and their associated values, essentially packaging up 'Host', 'Path', 'Method' and others with a value ('action') to represent an entry in your routing table.
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Web.Route.Invertible.Route
  ( RoutePredicate(..)
  , Route(..)
  , routeHost
  , routeSecure
  , routePath
  , routeMethod
  , routePriority
  , requestRoute'
  , requestRoute
  , Action(..)
  ) where

import Control.Invertible.Monoidal
import Control.Invertible.Monoidal.Free
import qualified Data.Invertible as I
import Data.Monoid (Endo(..))

import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path
import Web.Route.Invertible.Request

data RoutePredicate a where
  RouteHost     :: !(Host h) -> RoutePredicate h
  RouteSecure   :: !Bool     -> RoutePredicate ()
  RoutePath     :: !(Path p) -> RoutePredicate p
  RouteMethod   :: !Method   -> RoutePredicate ()
  RoutePriority :: !Int      -> RoutePredicate ()

newtype Route a = Route { freeRoute :: Free RoutePredicate a }
  deriving (I.Functor, Monoidal, MonoidalAlt)

routeHost :: Host h -> Route h
routeHost = Route . Free . RouteHost

routeSecure :: Bool -> Route ()
routeSecure = Route . Free . RouteSecure

routePath :: Path p -> Route p
routePath = Route . Free . RoutePath

routeMethod :: Method -> Route ()
routeMethod = Route . Free . RouteMethod

routePriority :: Int -> Route ()
routePriority = Route . Free . RoutePriority

predicateOrder :: RoutePredicate a -> Int
predicateOrder (RouteHost _)     = 1
predicateOrder (RouteSecure _)   = 2
predicateOrder (RoutePath _)     = 3
predicateOrder (RouteMethod _)   = 4
predicateOrder (RoutePriority _) = 5

{-
normalizeFree :: Free RoutePredicate a -> Free RoutePredicate a
normalizeFree Empty = Empty
normalizeFree (Transform f p) = Transform f $ normalizeFree p
normalizeFree (Join a@(RouteHost _) b@(RouteHost _)) = Transform f $ normalizeFree p
-}

requestRoutePredicate :: RoutePredicate a -> a -> Request -> Request
requestRoutePredicate (RouteHost (HostRev s)) h q = q{ requestHost = renderSequence s h }
requestRoutePredicate (RouteSecure s)        () q = q{ requestSecure = s }
requestRoutePredicate (RoutePath (Path s))    p q = q{ requestPath = renderSequence s p }
requestRoutePredicate (RouteMethod m)        () q = q{ requestMethod = m }
requestRoutePredicate (RoutePriority _)      () q = q

requestRoute' :: Route a -> a -> Request -> Request
requestRoute' (Route r) = appEndo . foldFree (\p -> Endo . requestRoutePredicate p) r

requestRoute :: Route a -> a -> Request
requestRoute r a = requestRoute' r a blankRequest

data Action a b = Action
  { actionRoute :: !(Route a)
  , routeAction :: !(a -> b)
  }

infix 1 `Action`

instance Functor (Action a) where
  fmap f (Action r a) = Action r $ f . a
