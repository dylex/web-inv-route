{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module Web.Route.Invertible.Map.Route
  ( RouteMap
  , singleton
  , fallbackHEADtoGET
  , lookup 
  ) where

import Prelude hiding (lookup)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import Network.HTTP.Types.Header (ResponseHeaders, hAllow)
import Network.HTTP.Types.Status (Status, notFound404, methodNotAllowed405)

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Method
import Web.Route.Invertible.Route
import Web.Route.Invertible.Request
import qualified Web.Route.Invertible.Map.Monoid as Monoid
import qualified Web.Route.Invertible.Map.Path as Path
import qualified Web.Route.Invertible.Map.Method as Method
import Web.Route.Invertible.Monoid.Exactly

newtype SubPathMap a = SubPathMap { subPathMap :: Method.MethodMap (Exactly a) }
  deriving (Monoid)

instance Functor SubPathMap where
  fmap f (SubPathMap m) = SubPathMap $ fmap (fmap f) m

newtype RouteMap a = RouteMap { routeMap :: Path.PathMap (SubPathMap (Path.PathValue -> a)) }
  deriving (Monoid)

instance Functor RouteMap where
  fmap f (RouteMap m) = RouteMap $ fmap (fmap (fmap f)) m

singleton :: Route p a -> RouteMap a
singleton Route{..} = RouteMap $
  Path.singletonApp routePath $ SubPathMap
  $ Monoid.MonoidMap $ Map.singleton routeMethod
  $ Exactly routeAction

fallbackHEADtoGET :: RouteMap a -> RouteMap a
fallbackHEADtoGET (RouteMap m) = RouteMap $ fmap (SubPathMap . Method.fallbackHEADtoGET . subPathMap) m

lookup :: Request -> RouteMap a -> Either (Status, ResponseHeaders) a
lookup q@Request{..} =
  either method exactly
  . Method.lookup requestMethod . subPathMap
  . Path.lookupApp requestPath . routeMap
  where
  method [] = notfound
  method m = Left (methodNotAllowed405, [(hAllow, BSC.intercalate (BSC.singleton ',') $ map renderParameter (m :: [Method]))])
  exactly Blank = notfound
  exactly (Exactly a) = Right a
  exactly Conflict = error $ "Conflict routing " ++ show q
  notfound = Left (notFound404, [])
