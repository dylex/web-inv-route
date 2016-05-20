-- |Single-route construction.
-- This package lets you describe the individual end-points for routing and their associated values, essentially packaging up 'Host', 'Path', 'Method' and others with a value ('Action') to represent an entry in your routing table.
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, DeriveDataTypeable, QuasiQuotes #-}
module Web.Route.Invertible.Route
  ( RoutePredicate(..)
  , Route(..)
  , routeHost
  , routeSecure
  , routePath
  , routeMethod
  , routeMethods
  , routePriority
  , normRoute
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

-- |A term, qualifier, or component of a route, each specifying one filter/attribute/parser/generator for a request.
data RoutePredicate a where
  RouteHost     :: !(Host h) -> RoutePredicate h
  RouteSecure   :: !Bool     -> RoutePredicate ()
  RoutePath     :: !(Path p) -> RoutePredicate p
  RouteMethod   :: !Method   -> RoutePredicate ()
  RoutePriority :: !Int      -> RoutePredicate ()

instance Show (RoutePredicate a) where
  showsPrec d (RouteHost h) = showParen (d > 10) $
    showString "RouteHost " . showsPrec 11 h
  showsPrec d (RouteSecure s) = showParen (d > 10) $
    showString "RouteSecure " . showsPrec 11 s
  showsPrec d (RoutePath p) = showParen (d > 10) $
    showString "RoutePath " . showsPrec 11 p
  showsPrec d (RouteMethod m) = showParen (d > 10) $
    showString "RouteMethod " . showsPrec 11 m
  showsPrec d (RoutePriority p) = showParen (d > 10) $
    showString "RoutePriority " . showsPrec 11 p

-- |A 'Monoidal' collection of routing predicates.
-- For example:
--
-- > routeHost ("www" >* "domain.com") *< routePath ("object" *< parameter) :: Route Int
newtype Route a = Route { freeRoute :: Free RoutePredicate a }
  deriving (I.Functor, Monoidal, MonoidalAlt)

instance Show (Route a) where
  showsPrec d (Route s) = showParen (d > 10) $
    showString "Route " . showsFree (showsPrec 11) s

-- |Limit a route to matching hosts.
-- By default, routes apply to any hosts not matched by any other routes in the map.
-- When combining (with 'Web.Route.Invertible.Map.Route.routes') or normalizing (with 'normRoute') routes, this has the highest precedence.
routeHost :: Host h -> Route h
routeHost = Route . Free . RouteHost

-- |Limit a route to only secure (https:) or insecure (http:) protocols.
-- By default, routes apply to both.
routeSecure :: Bool -> Route ()
routeSecure = Route . Free . RouteSecure

-- |Limit a route to matching paths.
-- By default, routes apply to any paths not matched by any other routes in the map (e.g., 404 handler, though it can be more general to handle a 'Web.Route.Invertible.Result.RouteNotFound' result directly) that also match all previous predicates. 
routePath :: Path p -> Route p
routePath = Route . Free . RoutePath

-- |Limit a route to a method.
-- By default, routes apply to all methods not handled by any other routes for the same earlier matching predicates (e.g., within the same path).
routeMethod :: IsMethod m => m -> Route ()
routeMethod = Route . Free . RouteMethod . toMethod

-- |Limit a route to a list of methods and return that method.
-- Supplying a method not in this list when generating (reverse) routes will result in a run-time error.
routeMethods :: (Eq m, IsMethod m) => [m] -> Route m
routeMethods [] = error "routeMethods: empty list"
routeMethods [m] = ((\() -> m) I.:<->: (\n -> if n == m then () else error ("routeMethods: unsupported method " ++ show (toMethod n)))) >$< routeMethod m
routeMethods (m:l) = (I.fromMaybe m I.. I.rgt) >$< (routeMethod m >|< routeMethods l)

-- |Set the priority of a route.  Routes with higher priority take precedence when there is a conflict.
-- By default, routes have priority 0.
-- When combining (with 'Web.Route.Invertible.Map.Route.routes') or normalizing (with 'normRoute') routes, this has the lowest precedence (so that conflicts are handled only after matching all other predicates).
routePriority :: Int -> Route ()
routePriority = Route . Free . RoutePriority

predicateOrder :: RoutePredicate a -> Int
predicateOrder (RouteHost     _) = 1
predicateOrder (RouteSecure   _) = 2
predicateOrder (RoutePath     _) = 3
predicateOrder (RouteMethod   _) = 4
predicateOrder (RoutePriority _) = 5

comparePredicate :: RoutePredicate a -> RoutePredicate b -> Ordering
comparePredicate p q = compare (predicateOrder p) (predicateOrder q)

-- |By default, route predicates are matched in the order they are specified, so each test is done only if all preceding tests succeed.
-- However, in most cases routing rules should be tested in a specific order in order to produce sensible errors (e.g., a 405 error that offers available methods should only apply to other routes with the same path).
-- This re-orders the predicates in a route in order of the constructors in 'RoutePredicate' (i.e., host, secure, path, method, ...), allowing you to construct your routes in any order but still produce sensible matching behavior.
-- Alternatively, since there are cases you may watch to match in a different order (e.g., for 'routePriority'), you can specify your routes in specific order and avoid this function (which would also be more efficient).
-- Note that there are some \"de-normalized\" cases that this will not correct, such as having duplicate 'routeMethod' specifications (in which case all must match, but each is done independently).
normRoute :: Route a -> Route a
normRoute = Route . sortFreeTDNF comparePredicate . freeRoute

requestRoutePredicate :: RoutePredicate a -> a -> Request -> Request
requestRoutePredicate (RouteHost (HostRev s)) h q = q{ requestHost = renderSequence s h }
requestRoutePredicate (RouteSecure s)        () q = q{ requestSecure = s }
requestRoutePredicate (RoutePath (Path s))    p q = q{ requestPath = renderSequence s p }
requestRoutePredicate (RouteMethod m)        () q = q{ requestMethod = m }
requestRoutePredicate (RoutePriority _)      () q = q

-- |Given an instantiation of a 'Route' with its value, add the relevant reverse-route information to a 'Request'.
requestRoute' :: Route a -> a -> Request -> Request
requestRoute' (Route r) = appEndo . foldFree (\p -> Endo . requestRoutePredicate p) r

-- |Apply 'requestRoute'' to 'blankRequest'.
requestRoute :: Route a -> a -> Request
requestRoute r a = requestRoute' r a blankRequest

-- |Specify the action to take for a given route, often used as an infix operator between the route specification and the function used to produce the result (which usually generates the HTTP response, but could be anything).
data Action a b = Action
  { actionRoute :: !(Route a)
  , routeAction :: !(a -> b)
  }

infix 1 `Action`

instance Functor (Action a) where
  fmap f (Action r a) = Action r $ f . a
