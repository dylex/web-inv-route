{-# LANGUAGE CPP #-}
-- |A compatibility routing layer for Snap applications.
module Web.Route.Invertible.Snap
  ( module Web.Route.Invertible.Common
  , snapRequest
  , routeSnap
  , routeMonadSnap
  ) where

import Control.Arrow (left)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as M
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.URI (decodePath)
import Network.HTTP.Types.Status (statusCode)
import qualified Snap.Core as Snap

import Web.Route.Invertible.Internal
import Web.Route.Invertible.Common
import Web.Route.Invertible

-- |Corvert a 'Snap.Request' to a request.
snapRequest :: Snap.Request -> Request
snapRequest q = Request
  { requestHost = splitHost $
#if MIN_VERSION_snap_core(1,0,0)
    Snap.rqHostName q
#else
    Snap.rqServerName q
#endif
  , requestSecure = Snap.rqIsSecure q
  , requestMethod = toMethod $ Snap.rqMethod q
  , requestPath = fst $ decodePath $ Snap.rqPathInfo q
  , requestQuery = HM.fromList $ M.toList $ Snap.rqQueryParams q
  , requestContentType = fromMaybe mempty $ Snap.getHeader hContentType q
  }

-- |Lookup a snap request in a route map, returning either an empty error response or a successful result.
routeSnap :: Snap.Request -> RouteMap a -> Either Snap.Response a
routeSnap q = left err . routeRequest (snapRequest q) where
  err (s, h) = foldr (\(n,v) -> Snap.setHeader n v)
    (Snap.setResponseCode (statusCode s) $ Snap.emptyResponse)
    h
  
-- |Combine a set of snap actions in a routing map into a single action, pre-setting an empty response.and returning Nothing in case of error.
routeMonadSnap :: Snap.MonadSnap m => RouteMap (m a) -> m (Maybe a)
routeMonadSnap m = do
  q <- Snap.getRequest
  either ((<$) Nothing . Snap.putResponse) (Just <$>) $ routeSnap q m
