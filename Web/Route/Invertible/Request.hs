module Web.Route.Invertible.Request
  ( Request(..)
  ) where

import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path

data Request = Request
  { requestHost :: [HostString]
  , requestMethod :: Method
  , requestPath :: [PathString]
  } deriving (Show)
