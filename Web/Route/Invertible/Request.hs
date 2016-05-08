-- |Representation of HTTP requests.
module Web.Route.Invertible.Request
  ( Request(..)
  , blankRequest
  ) where

import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path

data Request = Request
  { requestSecure :: Bool
  , requestHost :: [HostString]
  , requestMethod :: Method
  , requestPath :: [PathString]
  } deriving (Show)

blankRequest :: Request
blankRequest = Request
  { requestSecure = False
  , requestHost = []
  , requestMethod = ExtensionMethod mempty
  , requestPath = []
  }
