module Web.Route.Request
  ( Request(..)
  ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types.Method (StdMethod)

import Web.Route.Path.Internal

data Request = Request
  { requestMethod :: Either ByteString StdMethod
  , requestPath :: [PathString]
  } deriving (Show)
