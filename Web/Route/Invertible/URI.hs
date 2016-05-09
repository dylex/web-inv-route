{-# LANGUAGE RecordWildCards #-}
module Web.Route.Invertible.URI
  ( requestURI 
  , uriRequest
  , uriGETRequest
  , routeURI
  ) where

import Control.Arrow ((&&&))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Network.URI

import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
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
  }

-- |Convert a method and URI to a request.
uriRequest :: IsMethod m => m -> URI -> Request
uriRequest m u = Request
  { requestMethod = toMethod m
  , requestSecure = uriScheme u == "https:"
  , requestHost = maybe [] (splitHost . BSC.pack . uriRegName) $ uriAuthority u
  , requestPath = map (T.pack . unEscapeString) $ pathSegments u
  }

-- |Convert a GET URI to a request.
uriGETRequest :: URI -> Request
uriGETRequest = uriRequest GET

-- |Reverse a route to a URI.
routeURI :: RouteFunction h p a (Method, URI)
routeURI = mapRouteFunction (requestMethod &&& requestURI) requestRoute
