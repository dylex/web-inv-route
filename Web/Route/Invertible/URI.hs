{-# LANGUAGE RecordWildCards #-}
-- |
-- Conversion between "Network.URI" and routable representations such as 'Request'.
--
-- The most useful function here is 'routeActionURI' which performs reverse routing.
-- If you have an action already defined:
--
-- > getThing :: 'RouteAction' Int (IO Response)
--
-- Then @routeActionURI getThing 123@ will return the method and URI for that route, filling in the placeholders appropriately, e.g., @(GET, \"\/thing\/123\")@.
module Web.Route.Invertible.URI
  ( requestURI 
  , uriRequest
  , uriGETRequest
  , routeActionURI
  ) where

import Control.Arrow ((&&&))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Network.HTTP.Types.URI (parseSimpleQuery, renderSimpleQuery)
import Network.URI

import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Query
import Web.Route.Invertible.Request
import Web.Route.Invertible.Route

-- |Convert a request to a URI, ignoring the method.
requestURI :: Request -> URI
requestURI Request{..} = nullURI
  { uriScheme = if requestSecure then "https:" else "http:"
  , uriAuthority = if null requestHost then Nothing else Just URIAuth
    { uriUserInfo = ""
    , uriRegName = BSC.unpack $ joinHost requestHost
    , uriPort = ""
    }
  , uriPath = concatMap ((:) '/' . escapeURIString isUnescapedInURIComponent . T.unpack) requestPath
  , uriQuery = BSC.unpack $ renderSimpleQuery True $ paramsQuerySimple requestQuery
  }

-- |Convert a method and URI to a request.
uriRequest :: IsMethod m => m -> URI -> Request
uriRequest m u = Request
  { requestMethod = toMethod m
  , requestSecure = uriScheme u == "https:"
  , requestHost = maybe [] (splitHost . BSC.pack . uriRegName) $ uriAuthority u
  , requestPath = map (T.pack . unEscapeString) $ pathSegments u
  , requestQuery = simpleQueryParams $ parseSimpleQuery $ BSC.pack $ uriQuery u
  , requestContentType = mempty
  }

-- |Convert a GET URI to a request.
uriGETRequest :: URI -> Request
uriGETRequest = uriRequest GET

-- |Reverse a route action to a URI.
routeActionURI :: RouteAction r a -> r -> (Method, URI)
routeActionURI r = (requestMethod &&& requestURI) . requestActionRoute r
