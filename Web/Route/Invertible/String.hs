-- |Representations of (string) data that can be routed/parsed.
{-# LANGUAGE FlexibleInstances #-}
module Web.Route.Invertible.String
  ( RouteString(..)
  , PathString
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Hashable (Hashable(..))
import Data.String (IsString(..))
import qualified Data.Text as T

-- |Representions of request data that can be used in routing
class (Eq s, IsString s, Hashable s, Monoid s) => RouteString s where
  -- |Must be the inverse of 'fromString'
  toString :: s -> String

instance RouteString String where
  toString = id
instance RouteString T.Text where
  toString = T.unpack
instance RouteString BS.ByteString where
  toString = BS.unpack

type PathString = T.Text
