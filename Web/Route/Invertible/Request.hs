-- |Representation of HTTP requests.
module Web.Route.Invertible.Request
  ( Request(..)
  , blankRequest
  ) where

import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path
import Web.Route.Invertible.Query
import Web.Route.Invertible.ContentType

-- |A reduced representation of an HTTP request, sufficient for routing.
-- This lets us both pre-process/parse the request to optimize routing, and be agnostic about the incoming request representation.
-- These can be created with one of the framework-specific layers.
data Request = Request
  { requestSecure :: Bool
  , requestHost :: [HostString]
  , requestMethod :: Method
  , requestPath :: [PathString]
  , requestQuery :: QueryParams
  , requestContentType :: ContentType
  } deriving (Show, Eq)

-- |A blank/unknown request; effectively the default value
blankRequest :: Request
blankRequest = Request
  { requestSecure = False
  , requestHost = []
  , requestMethod = ExtensionMethod mempty
  , requestPath = []
  , requestQuery = mempty
  , requestContentType = mempty
  }
