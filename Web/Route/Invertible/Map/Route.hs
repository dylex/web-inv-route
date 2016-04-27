{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module Web.Route.Invertible.Map.Route
  ( RouteCase
  , RouteMap
  , routeCase
  , routes
  , fallbackHEADtoGET
  , routeRequest
  ) where

import Prelude hiding (lookup)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import Network.HTTP.Types.Header (ResponseHeaders, hAllow)
import Network.HTTP.Types.Status (Status, notFound404, methodNotAllowed405)

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Path
import Web.Route.Invertible.Method
import Web.Route.Invertible.Route
import Web.Route.Invertible.Request
import Web.Route.Invertible.Map.Monoid
import Web.Route.Invertible.Map.Const
import Web.Route.Invertible.Map.Default
import Web.Route.Invertible.Map.Sequence
import Web.Route.Invertible.Map.Path
import Web.Route.Invertible.Map.Method
import Web.Route.Invertible.Monoid.Exactly

newtype SubPathCase a = SubPathCase { subPathCase :: ConstMap MethodMap (Exactly a) }
  deriving (Monoid)

instance Functor SubPathCase where
  fmap f = SubPathCase . fmap (fmap f) . subPathCase

newtype RouteCase a = RouteCase { routeCaseMap :: PathMapApp SubPathCase a }
  deriving (Monoid)

instance Functor RouteCase where
  fmap f = RouteCase . fmap (fmap (fmap f)) . routeCaseMap

routeCase :: Route p a -> RouteCase a
routeCase Route{..} = RouteCase $
  singletonSequenceApp (pathSequence routePath) $ SubPathCase
  $ singletonConst ((MonoidMap .) . Map.singleton <$> routeMethod)
  $ Exactly routeAction

newtype SubPathMap a = SubPathMap { subPathMap :: DefaultMap MethodMap (Exactly a) }
  deriving (Monoid)

instance Functor SubPathMap where
  fmap f = SubPathMap . fmap (fmap f) . subPathMap

newtype RouteMap a = RouteMap { routeMap :: PathMapApp SubPathMap a }
  deriving (Monoid)

instance Functor RouteMap where
  fmap f = RouteMap . fmap (fmap (fmap f)) . routeMap

routes :: [RouteCase a] -> RouteMap a
routes = RouteMap . fmap (SubPathMap . flattenConstMap . subPathCase) . routeCaseMap . mconcat

fallbackHEADtoGET :: RouteMap a -> RouteMap a
fallbackHEADtoGET = RouteMap . fmap (SubPathMap . fallbackDefaultMethodHEADtoGET . subPathMap) . routeMap

routeRequest :: Request -> RouteMap a -> Either (Status, ResponseHeaders) a
routeRequest q@Request{..} =
  either method exactly
  . lookupDefaultMethod requestMethod . subPathMap
  . lookupSequenceApp requestPath . routeMap
  where
  method [] = notfound
  method m = Left (methodNotAllowed405, [(hAllow, BSC.intercalate (BSC.singleton ',') $ map renderParameter (m :: [Method]))])
  exactly Blank = notfound
  exactly (Exactly a) = Right a
  exactly Conflict = error $ "Conflict routing " ++ show q
  notfound = Left (notFound404, [])
