-- |Representation of HTTP request methods.
{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module Web.Route.Invertible.Method
  ( Method(..)
  , IsMethod(..)
  ) where

import Prelude hiding (lookup)

import Data.ByteString (ByteString)
import qualified Network.HTTP.Types.Method as H
#ifdef VERSION_snap_core
import qualified Snap.Core as Snap
#endif
#ifdef VERSION_happstack_server
import qualified Happstack.Server.Types as HS
#endif

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

-- |Any types that represent an HTTP method.
class IsMethod m where
  toMethod :: m -> Method
  fromMethod :: Method -> Maybe m

instance IsMethod Method where
  toMethod = id
  fromMethod = Just

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
  fromMethod GET = Just H.GET
  fromMethod POST = Just H.POST
  fromMethod HEAD = Just H.HEAD
  fromMethod PUT = Just H.PUT
  fromMethod DELETE = Just H.DELETE
  fromMethod TRACE = Just H.TRACE
  fromMethod CONNECT = Just H.CONNECT
  fromMethod OPTIONS = Just H.OPTIONS
  fromMethod PATCH = Just H.PATCH
  fromMethod _ = Nothing

instance IsMethod (Either ByteString H.StdMethod) where
  toMethod = either ExtensionMethod toMethod
  fromMethod (ExtensionMethod e) = Just $ Left e
  fromMethod m = Right <$> fromMethod m

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
  fromMethod = Just . renderParameter

#ifdef VERSION_snap_core
instance IsMethod Snap.Method where
  toMethod Snap.GET = GET
  toMethod Snap.HEAD = HEAD
  toMethod Snap.POST = POST
  toMethod Snap.PUT = PUT
  toMethod Snap.DELETE = DELETE
  toMethod Snap.TRACE = TRACE
  toMethod Snap.OPTIONS = OPTIONS
  toMethod Snap.CONNECT = CONNECT
  toMethod Snap.PATCH = PATCH
  toMethod (Snap.Method m) = ExtensionMethod m
  fromMethod GET = Just Snap.GET
  fromMethod HEAD = Just Snap.HEAD
  fromMethod POST = Just Snap.POST
  fromMethod PUT = Just Snap.PUT
  fromMethod DELETE = Just Snap.DELETE
  fromMethod TRACE = Just Snap.TRACE
  fromMethod OPTIONS = Just Snap.OPTIONS
  fromMethod CONNECT = Just Snap.CONNECT
  fromMethod PATCH = Just Snap.PATCH
  fromMethod (ExtensionMethod m) = Just $ Snap.Method m
#endif

#ifdef VERSION_happstack_server
instance IsMethod HS.Method where
  toMethod HS.GET = GET
  toMethod HS.HEAD = HEAD
  toMethod HS.POST = POST
  toMethod HS.PUT = PUT
  toMethod HS.DELETE = DELETE
  toMethod HS.TRACE = TRACE
  toMethod HS.OPTIONS = OPTIONS
  toMethod HS.CONNECT = CONNECT
  toMethod HS.PATCH = PATCH
  toMethod (HS.EXTENSION m) = ExtensionMethod m
  fromMethod GET = Just HS.GET
  fromMethod HEAD = Just HS.HEAD
  fromMethod POST = Just HS.POST
  fromMethod PUT = Just HS.PUT
  fromMethod DELETE = Just HS.DELETE
  fromMethod TRACE = Just HS.TRACE
  fromMethod OPTIONS = Just HS.OPTIONS
  fromMethod CONNECT = Just HS.CONNECT
  fromMethod PATCH = Just HS.PATCH
  fromMethod (ExtensionMethod m) = Just $ HS.EXTENSION m
#endif
