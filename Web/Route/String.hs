-- |Representations of (string) data that can be routed/parsed.
{-# LANGUAGE FlexibleInstances #-}
module Web.Route.String
  ( RouteString(..)
  ) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Hashable (Hashable(..))
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types.URI as H

-- |Representions of request data that can be used in routing
class (Eq s, IsString s, Hashable s, Monoid s, Show s) => RouteString s where
  -- |Must be the inverse of 'fromString'
  toString :: s -> String
  -- |Is this the empty string (i.e., 'mempty')?
  nullString :: s -> Bool
  nullString = (==) mempty
  -- |Encode a path (segment) in this representation, as @'H.urlEncodeBuilder' False@.
  urlEncodePath :: s -> B.Builder

instance RouteString String where
  toString = id
  nullString = null
  urlEncodePath = urlEncodePath . B.toLazyByteString . B.stringUtf8
instance RouteString T.Text where
  toString = T.unpack
  nullString = T.null
  urlEncodePath = urlEncodePath . TE.encodeUtf8
instance RouteString TL.Text where
  toString = TL.unpack
  nullString = TL.null
  urlEncodePath = urlEncodePath . TL.toStrict
instance RouteString BS.ByteString where
  toString = BS.unpack
  nullString = BS.null
  urlEncodePath = H.urlEncodeBuilder False
instance RouteString BSL.ByteString where
  toString = BSL.unpack
  nullString = BSL.null
  urlEncodePath = urlEncodePath . BSL.toStrict
