-- |A compatibility routing layer for Happstack applications.
module Web.Route.Invertible.Happstack
  ( module Web.Route.Invertible.Common
  , routeHappstack
  ) where

import Control.Arrow ((***), left)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Types.Header (hHost, hContentType)
import Network.HTTP.Types.Status (statusCode)
import qualified Happstack.Server.Types as HS

import Web.Route.Invertible.Internal
import Web.Route.Invertible.Common
import Web.Route.Invertible

happstackRequest :: HS.Request -> Request
happstackRequest q = Request
  { requestHost = maybe [] splitHost $ HS.getHeaderBS (CI.original hHost) q
  , requestSecure = HS.rqSecure q
  , requestMethod = toMethod $ HS.rqMethod q
  , requestPath = map T.pack $ HS.rqPaths q
  , requestQuery = simpleQueryParams $ map (BSC.pack *** either BSC.pack BSL.toStrict . HS.inputValue) $ HS.rqInputsQuery q
  , requestContentType = fromMaybe mempty $ HS.getHeaderBS (CI.original hContentType) q
  }

-- |Lookup a Happstack request in a route map, returning either an empty error response or a successful result.
routeHappstack :: HS.Request -> RouteMap a -> Either HS.Response a
routeHappstack q = left err . routeRequest (happstackRequest q) where
  err (s, h) = foldr (\(n,v) -> HS.setHeaderBS (CI.original n) v)
    (HS.resultBS (statusCode s) BSL.empty)
    h
