-- |A compatibility routing layer for WAI applications.
module Web.Route.Invertible.Wai
  ( module Web.Route.Invertible.Common
  , waiRequest
  , routeWai
  , routeWaiApplicationError
  , routeWaiApplication
  ) where

import Control.Arrow (second)
import Data.Maybe (fromMaybe)
import qualified Network.Wai as Wai
import Network.HTTP.Types.Header (ResponseHeaders, hContentType)
import Network.HTTP.Types.Status (Status)

import Web.Route.Invertible.Internal
import Web.Route.Invertible.Common
import Web.Route.Invertible

-- |Convert a 'Wai.Request' to a request.
waiRequest :: Wai.Request -> Request
waiRequest q = Request
  { requestHost = maybe [] splitHost $ Wai.requestHeaderHost q
  , requestSecure = Wai.isSecure q
  , requestMethod = toMethod $ Wai.requestMethod q
  , requestPath = Wai.pathInfo q
  , requestQuery = simpleQueryParams $ map (second $ fromMaybe mempty) $ Wai.queryString q
  , requestContentType = fromMaybe mempty $ lookup hContentType headers
  } where headers = Wai.requestHeaders q

-- |Lookup a wai request in a route map, returning either an error code and headers or a successful result.
routeWai :: Wai.Request -> RouteMap a -> Either (Status, ResponseHeaders) a
routeWai = routeRequest . waiRequest

-- |Combine a set of applications in a routing map into a single application, calling a custom error handler in case of routing error.
routeWaiApplicationError :: (Status -> ResponseHeaders -> Wai.Application) -> RouteMap Wai.Application -> Wai.Application
routeWaiApplicationError e m q = either (\(s, h) -> e s h q) (\a -> a q) $ routeWai q m

-- |Combine a set of applications in a routing map into a single application, returning an empty error response in case of routing error.
routeWaiApplication :: RouteMap Wai.Application -> Wai.Application
routeWaiApplication = routeWaiApplicationError $ \s h _ r -> r $ Wai.responseBuilder s h mempty
