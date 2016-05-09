module Web.Route.Invertible
  ( 
    -- * Route construction
    PathRoute
  , HostPathRoute
  , route
  , forHost
  , forSecure
  , forPath
  , forMethod
  , Method(..)
  , withPriority
  , action
    -- ** Parser construction
  , module Control.Invertible.Monoidal
  , wildcard
    -- ** Placeholder parameters
  , Parameter(..)
  , parameter
  , param
    -- * Reverse routing
  , requestRoute
    -- * Forward routing
  , RouteCase
  , routeCase
  , RouteMap
  , routes
  , Request(..)
  , routeRequest
  ) where

import Control.Invertible.Monoidal
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status)

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Method
import Web.Route.Invertible.Route
import Web.Route.Invertible.Request
import Web.Route.Invertible.Result
import Web.Route.Invertible.Map.Route

-- |Lookup a request in a routing table and transform it using 'routeResult'.
routeRequest :: Request -> RouteMap a -> Either (Status, ResponseHeaders) a
routeRequest q = routeResult . lookupRoute q
