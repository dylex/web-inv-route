module Web.Route.Invertible.Wai
  ( module Web.Route.Invertible.Common
  , routeWai
  , routeWaiApplicationError
  , routeWaiApplication
  ) where

import Control.Arrow (second)
import Data.Maybe (fromMaybe)
import qualified Network.Wai as Wai
import Network.HTTP.Types.Header (ResponseHeaders, hContentType)
import Network.HTTP.Types.Status (Status)

import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Query
import Web.Route.Invertible.Request
import Web.Route.Invertible.Common
import Web.Route.Invertible

waiRequest :: Wai.Request -> Request
waiRequest q = Request
  { requestHost = maybe [] splitHost $ Wai.requestHeaderHost q
  , requestSecure = Wai.isSecure q
  , requestMethod = toMethod $ Wai.requestMethod q
  , requestPath = Wai.pathInfo q
  , requestQuery = simpleQueryParams $ map (second $ fromMaybe mempty) $ Wai.queryString q
  , requestContentType = fromMaybe mempty $ lookup hContentType $ Wai.requestHeaders q
  }

routeWai :: Wai.Request -> RouteMap a -> Either (Status, ResponseHeaders) a
routeWai = routeRequest . waiRequest

routeWaiApplicationError :: (Status -> ResponseHeaders -> Wai.Application) -> RouteMap Wai.Application -> Wai.Application
routeWaiApplicationError e m q r = either (\(s, h) -> e s h q r) (\a -> a q r) $ routeWai q m

routeWaiApplication :: RouteMap Wai.Application -> Wai.Application
routeWaiApplication = routeWaiApplicationError $ \s h _ r -> r $ Wai.responseBuilder s h mempty
