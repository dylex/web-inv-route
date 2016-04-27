module Web.Route.Invertible.Wai
  ( routeWai
  , routeWaiApplicationError
  , routeWaiApplication
  ) where

import qualified Network.Wai as Wai
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status)

import Web.Route.Invertible.Method
import Web.Route.Invertible.Request
import Web.Route.Invertible.Map.Route

waiRequest :: Wai.Request -> Request
waiRequest q = Request
  { requestMethod = toMethod $ Wai.requestMethod q
  , requestPath = Wai.pathInfo q
  }

routeWai :: Wai.Request -> RouteMap a -> Either (Status, ResponseHeaders) a
routeWai = routeRequest . waiRequest

routeWaiApplicationError :: (Status -> ResponseHeaders -> Wai.Application) -> RouteMap Wai.Application -> Wai.Application
routeWaiApplicationError e m q r = either (\(s, h) -> e s h q r) (\a -> a q r) $ routeWai q m

routeWaiApplication :: RouteMap Wai.Application -> Wai.Application
routeWaiApplication = routeWaiApplicationError $ \s h _ r -> r $ Wai.responseBuilder s h mempty
