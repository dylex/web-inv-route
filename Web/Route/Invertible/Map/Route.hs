{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, StandaloneDeriving, GADTs #-}
module Web.Route.Invertible.Map.Route
  ( RouteCase
  , RouteMap
  , routeCase
  , routes
  , fallbackHEADtoGET
  , routeRequest
  ) where

import Prelude hiding (lookup)

import Control.Arrow (left)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import Data.List (nub)
import Data.Maybe (maybeToList)
import Network.HTTP.Types.Header (ResponseHeaders, hAllow)
import Network.HTTP.Types.Status (Status, notFound404, methodNotAllowed405)

import Web.Route.Invertible.String
import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Path
import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Route
import Web.Route.Invertible.Request
import Web.Route.Invertible.Map.Monoid
import Web.Route.Invertible.Map.Default
import Web.Route.Invertible.Map.Sequence
import Web.Route.Invertible.Map.Path
import Web.Route.Invertible.Map.Host
import Web.Route.Invertible.Map.Method
import Web.Route.Invertible.Monoid.Exactly
import Web.Route.Invertible.Monoid.Prioritized
import Web.Route.Invertible.Type

newtype RouteMap a = RouteMap { routeMap :: DefaultMap HostMap (DefaultMap PathMap (DefaultMap MethodMap (Prioritized (Exactly (HostPlaceholderValue -> PathPlaceholderValue -> a))))) }
  deriving (Monoid)

instance Functor RouteMap where
  fmap f = RouteMap . (fmap . fmap . fmap . fmap . fmap . fmap . fmap) f . routeMap

type RouteCase = RouteMap

hostCase :: TMaybe Host h -> (DefaultMap PathMap (DefaultMap MethodMap (Prioritized (Exactly (FromMaybeUnit h -> PathPlaceholderValue -> a))))) -> DefaultMap HostMap (DefaultMap PathMap (DefaultMap MethodMap (Prioritized (Exactly (HostPlaceholderValue -> PathPlaceholderValue -> a)))))
hostCase TNothing m = defaultingValue $ (fmap . fmap . fmap . fmap) (\f _ -> f ()) m
hostCase (TJust (HostRev s)) m = defaultingMap $ (\f -> (fmap . fmap . fmap . fmap) (. f) m) <$> singletonSequence s

pathCase :: TMaybe Path p -> (DefaultMap MethodMap (Prioritized (Exactly (h -> FromMaybeUnit p -> a)))) -> DefaultMap PathMap (DefaultMap MethodMap (Prioritized (Exactly (h -> PathPlaceholderValue -> a))))
pathCase TNothing m = defaultingValue $ (fmap . fmap . fmap) (\f h _ -> f h ()) m
pathCase (TJust (Path s)) m = defaultingMap $ (\f -> (fmap . fmap . fmap) (\g h -> g h . f) m) <$> singletonSequence s

routeCase :: Route h m p a -> RouteCase a
routeCase Route{..} = RouteMap $
  hostCase routeHost $
  pathCase routePath $
  when defaultingValue (((defaultingMap . MonoidMap) .) . Map.singleton) routeMethod $
  Prioritized routePriority $
  Exactly routeAction

routes :: [RouteCase a] -> RouteMap a
routes = mconcat

fallbackHEADtoGET :: RouteMap a -> RouteMap a
fallbackHEADtoGET = RouteMap . (fmap . fmap) fallbackDefaultMethodHEADtoGET . routeMap

lookupDefaultSequence :: RouteString s => [s] -> DefaultMap (SequenceMap s) a -> [(SequencePlaceholderValue, a)]
lookupDefaultSequence k (DefaultMap m d) = case lookupSequence k m of
  [] -> maybeToList $ (,) [] <$> d
  l -> l

mergeRight :: (Monoid a, Monoid b) => [Either a b] -> Either a b
mergeRight = foldr mr (Left mempty) where
  mr (Left x) = left (mappend x)
  mr (Right x) = Right . either (const x) (mappend x)

routeRequest :: Request -> RouteMap a -> Either (Status, ResponseHeaders) a
routeRequest q@Request{..} (RouteMap hm) =
  either method (exactly . prioritized) $
  mergeRight $ do
    (h, pm) <- lookupDefaultSequence requestHost hm
    (p, mm) <- lookupDefaultSequence requestPath pm
    return $ (fmap . fmap) (\f -> f h p) <$> lookupDefaultMethod requestMethod mm
  where
  method :: [Method] -> Either (Status, ResponseHeaders) a
  method [] = notfound
  method m = Left (methodNotAllowed405, [(hAllow, BSC.intercalate (BSC.singleton ',') $ map renderParameter $ nub m)])
  exactly Blank = notfound
  exactly (Exactly a) = Right a
  exactly Conflict = error $ "Conflict routing " ++ show q
  notfound = Left (notFound404, [])
