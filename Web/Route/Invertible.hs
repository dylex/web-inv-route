module Web.Route.Invertible
  ( routeRequest
  ) where

import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status)

import Web.Route.Invertible.Request
import Web.Route.Invertible.Result
import Web.Route.Invertible.Map.Route

routeRequest :: Request -> RouteMap a -> Either (Status, ResponseHeaders) a
routeRequest q = routeResult . lookupRoute q
