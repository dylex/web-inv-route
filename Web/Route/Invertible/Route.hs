-- |Single-route construction.
-- This package lets you describe the individual end-points for routing and their associated values, essentially packaging up 'Host', 'Path', 'Method' and others with a value ('Action') to represent an entry in your routing table.
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, DeriveDataTypeable, RankNTypes, TypeOperators, QuasiQuotes #-}
module Web.Route.Invertible.Route
  ( RoutePredicate(..)
  , Route(..)
  , routeHost
  , routeSecure
  , routePath
  , routeMethod
  , routeMethods
  , routeQuery
  , routeAccept
  , routeAccepts
  , routeCustom
  , routeFilter
  , routePriority
  , normRoute
  , foldRoute
  , requestRoute'
  , requestRoute
  , BoundRoute(..)
  , requestBoundRoute
  , RouteAction(..)
  , mapActionRoute
  , requestActionRoute
  , (!:?)
  ) where

import Control.Invertible.Monoidal
import Control.Invertible.Monoidal.Free
import Control.Monad (guard)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Invertible as I
import Data.Monoid (Endo(..))
import Data.Typeable (Typeable)

import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path
import Web.Route.Invertible.Query
import Web.Route.Invertible.ContentType
import Web.Route.Invertible.Request

-- |A term, qualifier, or component of a route, each specifying one filter/attribute/parser/generator for a request.
data RoutePredicate a where
  RouteHost     :: !(Host h) -> RoutePredicate h
  RouteSecure   :: !Bool     -> RoutePredicate ()
  RoutePath     :: !(Path p) -> RoutePredicate p
  RouteMethod   :: !Method   -> RoutePredicate ()
  RouteQuery    :: !QueryString -> !(Placeholder QueryString a) -> RoutePredicate a
  RouteAccept   :: !ContentType -> RoutePredicate ()
  RouteCustom   :: Typeable a => (Request -> Maybe a) -> (a -> Request -> Request) -> RoutePredicate a
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
  showsPrec d (RouteQuery q p) = showParen (d > 10) $
    showString "RouteQuery " . showsPrec 11 q . showString " " . showsPrec 11 p
  showsPrec d (RouteAccept t) = showParen (d > 10) $
    showString "RouteAccept " . showsPrec 11 t
  showsPrec d (RouteCustom _ _) = showParen (d > 10) $
    showString "RouteCustom <function> <function>"
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
routeMethods = oneOfI routeMethod

-- |Limit a route to requests with a matching URL query parameter.
-- By default, other routes match only when the given parameter is missing.
routeQuery :: QueryString -> Placeholder QueryString a -> Route a
routeQuery q = Route . Free . RouteQuery q

-- |Limit a route to requests with the given \"Content-type\" header, i.e., POST requests containing a request body of a certain type.
-- Note that this does not relate to the type of the response or the \"Accept\" header.
-- By default, routes match only requests without bodies or with content-type headers not matched by any other routes.
routeAccept :: ContentType -> Route ()
routeAccept = Route . Free . RouteAccept

-- |Limit a route to a list of methods and return that method.
-- Supplying a method not in this list when generating (reverse) routes will result in a run-time error.
routeAccepts :: [ContentType] -> Route ContentType
routeAccepts = oneOfI routeAccept

-- |A custom routing predicate that can perform arbitrary tests on the request and reverse routing.
-- The first argument is used in forward routing to check the request, and only passes if it returns 'Just'.
-- The second argument is used in reverse routing to modify the request according to the parameter.
-- By default, routes match all requests -- unlike other predicates, matching a custom rule does not exclude other routes.
-- This should be used sparingly and towards the end of a route as, unlike most other predicates, it only provides /O(n)/ lookups, as these functions must be called for every route candidate (those where all previous predicates match).
routeCustom :: Typeable a => (Request -> Maybe a) -> (a -> Request -> Request) -> Route a
routeCustom fwd rev = Route $ Free $ RouteCustom fwd rev

-- |A simpler version of 'routeCustom' that just takes a filter function to check again the request.
routeFilter :: (Request -> Bool) -> Route ()
routeFilter f = routeCustom (guard . f) (\() -> id)

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
predicateOrder (RouteQuery _  _) = 5
predicateOrder (RouteAccept   _) = 6
predicateOrder (RouteCustom _ _) = 7
predicateOrder (RoutePriority _) = 8

comparePredicate :: RoutePredicate a -> RoutePredicate b -> Ordering
comparePredicate (RouteQuery p _) (RouteQuery q _) = compare p q
comparePredicate p q = compare (predicateOrder p) (predicateOrder q)

-- |By default, route predicates are matched in the order they are specified, so each test is done only if all preceding tests succeed.
-- However, in most cases routing rules should be tested in a specific order in order to produce sensible errors (e.g., a 405 error that offers available methods should only apply to other routes with the same path).
-- This re-orders the predicates in a route in order of the constructors in 'RoutePredicate' (i.e., host, secure, path, method, ...), allowing you to construct your routes in any order but still produce sensible matching behavior.
-- Alternatively, since there are cases you may watch to match in a different order (e.g., for 'routePriority'), you can specify your routes in specific order and avoid this function (which would also be more efficient).
-- Note that there are some \"de-normalized\" cases that this will not correct, such as having duplicate 'routeMethod' specifications (in which case all must match, but each is done independently).
normRoute :: Route a -> Route a
normRoute = Route . sortFreeTDNF comparePredicate . freeRoute

-- |Fold over the predicates in an instatiated route.
foldRoute :: Monoid b => (forall a' . RoutePredicate a' -> a' -> b) -> Route a -> a -> b
foldRoute f (Route r) = foldFree f r

requestRoutePredicate :: RoutePredicate a -> a -> Request -> Request
requestRoutePredicate (RouteHost (HostRev s)) h q = q{ requestHost = renderSequence s h }
requestRoutePredicate (RouteSecure s)        () q = q{ requestSecure = s }
requestRoutePredicate (RoutePath (Path s))    p q = q{ requestPath = renderSequence s p }
requestRoutePredicate (RouteMethod m)        () q = q{ requestMethod = m }
requestRoutePredicate (RouteQuery n p)        v q = q{ requestQuery = HM.insertWith (++) n [renderPlaceholder p v] $ requestQuery q }
requestRoutePredicate (RouteAccept t)        () q = q{ requestContentType = t }
requestRoutePredicate (RouteCustom _ f)       a q = f a q
requestRoutePredicate (RoutePriority _)      () q = q

-- |Given an instantiation of a 'Route' with its value, add the relevant reverse-route information to a 'Request'.
requestRoute' :: Route a -> a -> Request -> Request
requestRoute' r = appEndo . foldRoute (\p -> Endo . requestRoutePredicate p) r

-- |Apply 'requestRoute'' to 'blankRequest'.
requestRoute :: Route a -> a -> Request
requestRoute r a = requestRoute' r a blankRequest

-- |A route bound with its parameter.  Useful for passing concerete specific routes without type variables.
data BoundRoute = forall a. Route a :? a

infix 1 :?

-- |Apply 'requestRoute' on a 'BoundRoute'.
requestBoundRoute :: BoundRoute -> Request
requestBoundRoute (r :? a) = requestRoute r a

-- |Specify the action to take for a given route, often used as an infix operator between the route specification and the function used to produce the result (which usually generates the HTTP response, but could be anything).
data RouteAction a b = RouteAction
  { actionRoute :: !(Route a)
  , routeAction :: !(a -> b)
  }

infix 1 `RouteAction`

instance Functor (RouteAction a) where
  fmap f (RouteAction r a) = RouteAction r $ f . a

-- |'RouteAction' is invariant in its first argument.
-- Apply a bijection to the routing argument, leaving the action alone.
mapActionRoute :: (a I.<-> b) -> RouteAction a r -> RouteAction b r
mapActionRoute f (RouteAction r a) = RouteAction (f >$< r) (a . I.biFrom f)

-- |Apply 'requestRoute' to 'actionRoute'.
requestActionRoute :: RouteAction a b -> a -> Request
requestActionRoute = requestRoute . actionRoute

-- |Combine '(:?)' and 'actionRoute'.
(!:?) :: RouteAction a b -> a -> BoundRoute
(!:?) = (:?) . actionRoute

infix 1 !:?
