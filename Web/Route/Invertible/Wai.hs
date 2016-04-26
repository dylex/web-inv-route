module Web.Route.Invertible.Wai
  ( routeWai
  , routeWaiApplication
  ) where

import qualified Network.Wai as Wai
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status)

import Web.Route.Invertible.Method
import Web.Route.Invertible.Request
import qualified Web.Route.Invertible.Map.Route as RM

waiRequest :: Wai.Request -> Request
waiRequest q = Request
  { requestMethod = toMethod $ Wai.requestMethod q
  , requestPath = Wai.pathInfo q
  }

routeWai :: Wai.Request -> RM.RouteMap a -> Either (Status, ResponseHeaders) a
routeWai = RM.lookup . waiRequest

routeWaiApplication :: RM.RouteMap Wai.Application -> Wai.Application
routeWaiApplication m q r = either (\(s, h) -> r $ Wai.responseBuilder s h mempty) (\a -> a q r) $ routeWai q m
