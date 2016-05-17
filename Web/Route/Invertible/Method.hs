-- |Representation of HTTP request methods.
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module Web.Route.Invertible.Method
  ( Method(..)
  , IsMethod(..)
  ) where

import Prelude hiding (lookup)

import Data.ByteString (ByteString)
import qualified Network.HTTP.Types.Method as H

import Web.Route.Invertible.Parameter

-- |Standard HTTP methods.
-- These are defined a number of places already, but the http-types version (which is the only thing we import by default) is too cumbersome.
data Method
  = OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  | CONNECT
  | PATCH
  | ExtensionMethod !ByteString
  deriving (Eq, Ord, Read, Show)

-- |Any types that represent an HTTP method.
class IsMethod m where
  toMethod :: m -> Method

instance IsMethod Method where
  toMethod = id

instance IsMethod H.StdMethod where
  toMethod H.GET = GET
  toMethod H.POST = POST
  toMethod H.HEAD = HEAD
  toMethod H.PUT = PUT
  toMethod H.DELETE = DELETE
  toMethod H.TRACE = TRACE
  toMethod H.CONNECT = CONNECT
  toMethod H.OPTIONS = OPTIONS
  toMethod H.PATCH = PATCH

instance IsMethod (Either ByteString H.StdMethod) where
  toMethod = either ExtensionMethod toMethod

instance IsMethod ByteString where
  toMethod "OPTIONS" = OPTIONS
  toMethod "GET" = GET
  toMethod "HEAD" = HEAD
  toMethod "POST" = POST
  toMethod "PUT" = PUT
  toMethod "DELETE" = DELETE
  toMethod "TRACE" = TRACE
  toMethod "CONNECT" = CONNECT
  toMethod "PATCH" = PATCH
  toMethod m = ExtensionMethod m

instance Parameter ByteString Method where
  parseParameter = Just . toMethod
  renderParameter OPTIONS = "OPTIONS"
  renderParameter GET = "GET"
  renderParameter HEAD = "HEAD"
  renderParameter POST = "POST"
  renderParameter PUT = "PUT"
  renderParameter DELETE = "DELETE"
  renderParameter TRACE = "TRACE"
  renderParameter CONNECT = "CONNECT"
  renderParameter PATCH = "PATCH"
  renderParameter (ExtensionMethod m) = m
