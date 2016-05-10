{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators #-}
module Web.Route.Invertible.HRoute
  ( Route(..)
  , Action(..)
  , requestRoute'
  , requestRoute
  ) where

import Data.HList.HList (HList(..))

import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path
import Web.Route.Invertible.Request

data Route (l :: [*]) where
  Route :: Route '[]
  RouteHost :: !(Route l) -> !(Host h) -> Route (h ': l)
  RouteSecure :: !(Route l) -> !Bool -> Route l
  RoutePath :: !(Route l) -> !(Path p) -> Route (p ': l)
  RouteMethod :: !(Route l) -> !Method -> Route l
  RoutePriority :: !(Route l) -> !Int -> Route l

data Action l a = Action
  { actionRoute :: !(Route l)
  , routeAction :: !(HList l -> a)
  }

requestRoute' :: Route l -> HList l -> Request -> Request
requestRoute' Route HNil q = q
requestRoute' (RouteHost r (HostRev s)) (HCons h l) q = (requestRoute' r l q){ requestHost = renderSequence s h }
requestRoute' (RouteSecure r s)         l           q = (requestRoute' r l q){ requestSecure = s }
requestRoute' (RoutePath r (Path s))    (HCons p l) q = (requestRoute' r l q){ requestPath = renderSequence s p }
requestRoute' (RouteMethod r m)         l           q = (requestRoute' r l q){ requestMethod = m }
requestRoute' (RoutePriority r _)       l           q = (requestRoute' r l q)

requestRoute :: Route l -> HList l -> Request
requestRoute r l = requestRoute' r l blankRequest

infixl 0 `RouteHost`, `RouteSecure`, `RoutePath`, `RouteMethod`, `RoutePriority`, `Action`
