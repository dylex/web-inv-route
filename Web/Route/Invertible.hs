module Web.Route.Invertible
  ( module Web.Route.Invertible.Common
  , normRoute
    -- * Request representation
  , HostString
  , splitHost
  , joinHost
  , PathString
  , normalizePath
  , Method(..)
  , IsMethod(..)
  , QueryParams
  , Request(..)
  , blankRequest
    -- * Reverse routing
  , requestRoute'
    -- * Forward routing
  , RouteResult(..)
  , lookupRoute
  , routeRequest
  ) where

import Control.Invertible.Monoidal
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status)

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Path
import Web.Route.Invertible.Method
import Web.Route.Invertible.Query
import Web.Route.Invertible.Route
import Web.Route.Invertible.Request
import Web.Route.Invertible.Result
import Web.Route.Invertible.Map.Route
import Web.Route.Invertible.Common

-- |Lookup a request in a routing table and transform it using 'routeResult'.
routeRequest :: Request -> RouteMap a -> Either (Status, ResponseHeaders) a
routeRequest q = routeResult . lookupRoute q
