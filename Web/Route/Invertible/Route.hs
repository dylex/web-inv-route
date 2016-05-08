{-# LANGUAGE FlexibleInstances, DataKinds, KindSignatures, RecordWildCards, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies #-}
module Web.Route.Invertible.Route
  ( Route(..)
  , PathRoute
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

-- |A single routing endpoint, representing the subset of requests handled and resulting action this route evaluates to.
-- Routes should usually be assigned to (top-level) variables using 'route', the various @forX@ functions, and finally `action`, e.g.:
--
-- > getThing :: PathRoute ThingId ...
-- > getThing = route `forMethod` GET `forPath` "thing" *< parameter `action` \thingId -> do ...
--
-- Routes are matched in the order of the constructors, so that a default path matches only once the host is matched, but a default host matches regardless of path (but of course all qualifiations must match for the route to apply).
data Route (h :: Maybe *) (p :: Maybe *) (m :: Bool) a = Route
  { routeHost :: !(TMaybe Host h)
  , routeSecure :: !(Maybe Bool)
  , routePath :: !(TMaybe Path p)
  , routeMethod :: !(When m Method)
  , routePriority :: !Int
  , routeAction :: FromMaybeVoid h -> FromMaybeVoid p -> a
  }

instance Functor (Route h p m) where
  fmap f Route{ routeAction = a, .. } = 
    Route{ routeAction = \h p -> f $ a h p, .. }

type PathRoute p = Route 'Nothing ('Just p) 'True

-- |A blank 'Route', the starting point to construct complete routes.
route :: Route 'Nothing 'Nothing 'False Void
route = Route TNothing Nothing TNothing WhenNot 0 $ \_ _ -> void

-- |Qualify a route with a 'Host' parser for virtual hosting.
-- By default, if no host is specified, a route matches all hosts not handled by other routes.
forHost :: Route 'Nothing p m a -> Host h -> Route ('Just h) p m a
forHost Route{ routeAction = f, .. } h =
  Route{ routeHost = TJust h, routeAction = \_ -> f void, .. }

-- |Qualify a route to only secure (https) or non-secure (http) requests.
-- By default, a route matches both.  This values overrides any previous values, such that:
--
-- > route `forSecure` False `forSecure` True
--
-- only matches secure requests.
forSecure :: Route h p m a -> Bool -> Route h p m a 
forSecure r s = r{ routeSecure = Just s }

-- |Qualify a route with a 'Path' parser.
-- By default, if no path is specified, a route matches all paths not handled by other routes (within the same host).
-- This could be used as a not-found handler, though you can also explicitly handle 'Web.Route.Invertible.Result.RouteNotFound'.
forPath :: Route h 'Nothing m a -> Path p -> Route h ('Just p) m a
forPath Route{ routeAction = f, .. } p =
  Route{ routePath = TJust p, routeAction = \h _ -> f h void, .. }

-- |Qualify a route for a specific 'Method'.
-- By default, if no method is specified, a route matches all methods not handled by other routes (within the same path).
forMethod :: IsMethod m => Route h p 'False a -> m -> Route h p 'True a
forMethod Route{ .. } m =
  Route{ routeMethod = WhenSo $ toMethod m, .. }

-- |Add a priority to a route.
-- If multiple routes match the same request, and one has a higher priority than the others, it will win.
-- If no route has a strictly higher priority, it is an error ('Web.Route.Invertible.Result.MultipleRoutes').
-- By default, if no priority is specified, routes have priority 0.
withPriority :: Route h p m a -> Int -> Route h p m a
withPriority r p = r{ routePriority = p }

-- |Specify the associated result or action to take when a route matches.
-- The function will be called with the parsed host and path values (if any), and the result will be returned (as 'Web.Route.Invertible.Result.RouteResult').
action :: Route h p m Void -> MaybeFunction h (MaybeFunction p a) -> Route h p m a
action Route{ .. } f =
  Route{ routeAction = tmaybeApply routePath . tmaybeApply routeHost f, .. }

requestRoute_ :: Route h p m a -> FromMaybeVoid h -> FromMaybeVoid p -> Request -> Request
requestRoute_ Route{..} h p Request{..} = Request
  { requestHost = tmaybe requestHost (\(HostRev s) -> renderSequence s h) routeHost
  , requestSecure = fromMaybe requestSecure routeSecure
  , requestPath = tmaybe requestPath (\(Path s) -> renderSequence s p) routePath
  , requestMethod = when requestMethod id routeMethod
  }

-- |Modify a request according to an instantiated route.
-- It takes arguments for the components of the route (e.g., host, path), and modifies the request, only if those components are specified.  That is:
--
-- > requestRoute' 'route' == id
requestRoute' :: Route h p m a -> MaybeFunction h (MaybeFunction p (Request -> Request))
requestRoute' r = tmaybeFunction (routeHost r) $ \h -> tmaybeFunction (routePath r) $ \p -> requestRoute_ r h p

-- |Apply 'requestRoute'' to 'blankRequest'.
-- This performs reverse routing: it gives you a filled-out request based on a single route endpoint and its parameters.
requestRoute :: Route h p m a -> MaybeFunction h (MaybeFunction p Request)
requestRoute r = tmaybeFunction (routeHost r) $ \h -> tmaybeFunction (routePath r) $ \p -> requestRoute_ r h p blankRequest

infixl 0 `forHost`, `forSecure`, `forPath`, `forMethod`, `withPriority`, `action`
