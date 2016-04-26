module Web.Route.Invertible.Request
  ( Request(..)
  ) where

import Web.Route.Invertible.Path
import Web.Route.Invertible.Method

data Request = Request
  { requestMethod :: Method
  , requestPath :: [PathString]
  } deriving (Show)
