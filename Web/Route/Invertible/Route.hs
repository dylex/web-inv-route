-- |Single-route construction.
-- This package lets you describe the individual end-points for routing and their associated values, essentially packaging up 'Host', 'Path', 'Method' and others with a value ('action') to represent an entry in your routing table.
{-# LANGUAGE FlexibleInstances, DataKinds, KindSignatures, RecordWildCards, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies #-}
module Web.Route.Invertible.Route
  ( Route(..)
  , RouteFunction
  , mapRouteFunction
  , BasicRoute
  , PathRoute
  , HostRoute
  , HostPathRoute
  , route
  , forHost
  , forSecure
  , forMethod
  , forPath
  , withPriority
  , action
  , requestRoute'
  , requestRoute
  ) where

import Data.Maybe (fromMaybe)
import Data.Void (Void)

import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path
import Web.Route.Invertible.Request
import Web.Route.Invertible.Type

-- |A function that takes (optional) arguments for host and path parameters.
type ParamFunction h p a = MaybeFunction h (MaybeFunction p a)

-- |A route function that also takes (optional) arguments for host and path parameters if the route includes them.
type RouteFunction h p a b = Route h p a -> ParamFunction h p b

-- |Convert a route function to its expanded void-argument version.
_unRouteFunction :: RouteFunction h p x a -> Route h p x -> FromMaybeVoid h -> FromMaybeVoid p -> a
_unRouteFunction f r = tmaybeApply (routePath r) . tmaybeApply (routeHost r) (f r)

-- |Convert a normal function with possibly void arguments into a route function.
routeFunction :: (Route h p x -> FromMaybeVoid h -> FromMaybeVoid p -> a) -> RouteFunction h p x a
routeFunction f r = tmaybeFunction (routeHost r) $ tmaybeFunction (routePath r) . f r

-- |Map over the output of a route function.
mapRouteFunction :: (a -> b) -> RouteFunction h p x a -> RouteFunction h p x b
mapRouteFunction f g r = tmaybeMapFunction (routeHost r) (tmaybeMapFunction (routePath r) f) $ g r

-- |A single routing endpoint, representing the subset of requests handled and resulting action this route evaluates to.
-- Routes should usually be assigned to (top-level) variables using 'route', the various @forX@ functions, and finally `action`, e.g.:
--
-- > getThing :: PathRoute ThingId ...
-- > getThing = route `forMethod` GET `forPath` "thing" *< parameter `action` \thingId -> do ...
--
-- Routes are matched in the order of the fields, so that a default path matches only once the host is matched, but a default host matches regardless of path (but of course ultimately all qualifiations must match for the route to apply).
data Route (h :: Maybe *) (p :: Maybe *) a = Route
  { routeHost :: !(TMaybe Host h) -- ^Optional host parser (default to any unhandled host)
  , routeSecure :: !(Maybe Bool) -- ^Optional secure filter (default to both)
  , routePath :: !(TMaybe Path p) -- ^Optional apth parser (default to any unhandled path)
  , routeMethod :: ![Method] -- ^Methods handled (default to all)
  , routePriority :: !Int -- ^Priority of this route (for "Web.Route.Invertible.Monoid.Prioritized")
  , routeAction :: FromMaybeVoid h -> FromMaybeVoid p -> a -- ^The resulting action
  }

instance Functor (Route h p) where
  fmap f Route{ routeAction = a, .. } = 
    Route{ routeAction = \h p -> f $ a h p, .. }

-- |A route that doesn't match on any parameterized qualifiers
type BasicRoute = Route 'Nothing 'Nothing
-- |A route that matches on path
type PathRoute p = Route 'Nothing ('Just p)
-- |A route that matches on host
type HostRoute h = Route ('Just h) 'Nothing
-- |A route that matches on host and path
type HostPathRoute h p = Route ('Just h) ('Just p)

-- |A blank 'Route', the starting point to construct complete routes.
route :: BasicRoute Void
route = Route TNothing Nothing TNothing [] 0 $ \_ _ -> void

-- |Qualify a route with a 'Host' parser for virtual hosting.
-- By default, if no host is specified, a route matches all hosts not handled by other routes.
forHost :: Route 'Nothing p a -> Host h -> Route ('Just h) p a
forHost Route{ routeAction = f, .. } h =
  Route{ routeHost = TJust h, routeAction = \_ -> f void, .. }

-- |Qualify a route to only secure (https) or non-secure (http) requests.
-- By default, a route matches both.
-- This values overrides any previous values, such that:
--
-- > route `forSecure` False `forSecure` True
--
-- only matches secure requests.
forSecure :: Route h p a -> Bool -> Route h p a 
forSecure r s = r{ routeSecure = Just s }

-- |Qualify a route with a 'Path' parser.
-- By default, if no path is specified, a route matches all paths not handled by other routes (within the same host).
-- This could be used as a not-found handler, though you can also explicitly handle 'Web.Route.Invertible.Result.RouteNotFound'.
forPath :: Route h 'Nothing a -> Path p -> Route h ('Just p) a
forPath Route{ routeAction = f, .. } p =
  Route{ routePath = TJust p, routeAction = \h _ -> f h void, .. }

-- |Qualify a route for a specific 'Method'.
-- By default, if no method is specified, a route matches all methods not handled by other routes (within the same path).
-- When multiple methods are specified, the route matches any of them, but the last one takes precedence for reverse routing.
forMethod :: IsMethod m => Route h p a -> m -> Route h p a
forMethod r m = r{ routeMethod = toMethod m : routeMethod r }

-- |Add a priority to a route.
-- If multiple routes match the same request, and one has a higher priority than the others, it will win.
-- If no route has a strictly higher priority, it is an error ('Web.Route.Invertible.Result.MultipleRoutes').
-- By default, if no priority is specified, routes have priority 0.
withPriority :: Route h p a -> Int -> Route h p a
withPriority r p = r{ routePriority = p }

-- |Specify the associated result or action to take when a route matches.
-- The function will be called with the parsed host and path values (if any), and the result will be returned (as 'Web.Route.Invertible.Result.RouteResult').
action :: Route h p Void -> ParamFunction h p a -> Route h p a
action Route{ .. } f =
  Route{ routeAction = tmaybeApply routePath . tmaybeApply routeHost f, .. }

requestRoute_ :: Route h p a -> FromMaybeVoid h -> FromMaybeVoid p -> Request -> Request
requestRoute_ Route{..} h p Request{..} = Request
  { requestHost = tmaybe requestHost (\(HostRev s) -> renderSequence s h) routeHost
  , requestSecure = fromMaybe requestSecure routeSecure
  , requestPath = tmaybe requestPath (\(Path s) -> renderSequence s p) routePath
  , requestMethod = case routeMethod of { [] -> requestMethod ; (m:_) -> m }
  }

-- |Modify a request according to an instantiated route.
-- It takes arguments for the components of the route (e.g., host, path), and modifies the request, only if those components are specified.  That is:
--
-- > requestRoute' 'route' == id
requestRoute' :: RouteFunction h p a (Request -> Request)
requestRoute' = routeFunction requestRoute_

-- |Apply 'requestRoute'' to 'blankRequest'.
-- This performs reverse routing: it gives you a filled-out request based on a single route endpoint and its parameters.
requestRoute :: RouteFunction h p a Request
-- requestRoute = mapRouteFunction ($ blankRequest) requestRoute'
requestRoute = routeFunction $ \r h p -> requestRoute_ r h p blankRequest

infixl 0 `forHost`, `forSecure`, `forPath`, `forMethod`, `withPriority`, `action`
