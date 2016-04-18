module Web.Route.Request
  ( Request(..)
  ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types.Method (Method)

data Request s = Request
  { requestMethod :: Method -- Either ByteString StdMethod
  , requestPath :: [s]
  } deriving (Show)
