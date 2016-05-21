module Web.Route.Invertible.ContentType
  ( ContentType
  ) where

import qualified Data.ByteString as BS

-- |String representation of content types, e.g., from HTTP \"Content-type\" headers.
type ContentType = BS.ByteString
