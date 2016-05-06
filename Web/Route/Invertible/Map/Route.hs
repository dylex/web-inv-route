{-# LANGUAGE DeriveFunctor, RecordWildCards, GADTs, Rank2Types #-}
module Web.Route.Invertible.Map.Route
  ( RouteCase
  , RouteMap
  , routeCase
  , routes
  , fallbackHEADtoGET
  , RouteResult(..)
  , lookupRoute
  , routeRequest
  ) where

import Prelude hiding (lookup)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
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
import Web.Route.Invertible.Map.Bool
import Web.Route.Invertible.Map.Sequence
import Web.Route.Invertible.Map.Path
import Web.Route.Invertible.Map.Host
import Web.Route.Invertible.Map.Method
import Web.Route.Invertible.Monoid.Exactly
import Web.Route.Invertible.Monoid.Prioritized
import Web.Route.Invertible.Type

data RouteMap a
  -- These constructors are expected to be nested in reverse order only, but we'd need some fancy type nat constraints to enforce this, and nothing really breaks that badly if they're not
  = RouteExactly   !(Exactly a)
  | RoutePriority  !(Prioritized (RouteMap a))
  | RouteMethod    !(DefaultMap MethodMap (RouteMap a))
  | RoutePath      !(DefaultMap PathMap (RouteMap (PathPlaceholderValue -> a)))
  | RouteSecure    !(BoolMap (RouteMap a))
  | RouteHost      !(DefaultMap HostMap (RouteMap (HostPlaceholderValue -> a)))
  deriving (Functor)

mapRoutes :: (forall b . RouteMap b -> RouteMap b) -> RouteMap a -> RouteMap a
mapRoutes f (RouteHost     m) = RouteHost     $ fmap f m
mapRoutes f (RouteSecure   m) = RouteSecure   $ fmap f m
mapRoutes f (RoutePath     m) = RoutePath     $ fmap f m
mapRoutes f (RouteMethod   m) = RouteMethod   $ fmap f m
mapRoutes f (RoutePriority m) = RoutePriority $ fmap f m
mapRoutes _ r@(RouteExactly _) = r

instance Monoid (RouteMap a) where
  mempty = RouteExactly Blank
  mappend (RouteExactly  a) (RouteExactly  b) = RouteExactly  (mappend a b)
  mappend (RoutePriority a) (RoutePriority b) = RoutePriority (mappend a b)
  mappend (RouteMethod   a) (RouteMethod   b) = RouteMethod   (mappend a b)
  mappend (RoutePath     a) (RoutePath     b) = RoutePath     (mappend a b)
  mappend (RouteSecure   a) (RouteSecure   b) = RouteSecure   (mappend a b)
  mappend (RouteHost     a) (RouteHost     b) = RouteHost     (mappend a b)
  mappend   a@(RouteExactly  _) b = mappend   (RoutePriority (Prioritized 0 a)) b
  mappend a b@(RouteExactly  _)   = mappend a (RoutePriority (Prioritized 0 b))
  mappend   a@(RoutePriority _) b = mappend   (RouteMethod (defaultingValue a)) b
  mappend a b@(RoutePriority _)   = mappend a (RouteMethod (defaultingValue b))
  mappend   a@(RouteMethod   _) b = mappend   (RoutePath   (defaultingValue $ fmap const a)) b
  mappend a b@(RouteMethod   _)   = mappend a (RoutePath   (defaultingValue $ fmap const b))
  mappend   a@(RoutePath     _) b = mappend   (RouteSecure (singletonBool Nothing a)) b
  mappend a b@(RoutePath     _)   = mappend a (RouteSecure (singletonBool Nothing b))
  mappend   a@(RouteSecure   _) b = mappend   (RouteHost   (defaultingValue $ fmap const a)) b
  mappend a b@(RouteSecure   _)   = mappend a (RouteHost   (defaultingValue $ fmap const b))

type RouteCase = RouteMap

hostCase :: TMaybe Host h -> RouteMap (FromMaybeUnit h -> a) -> RouteMap a
hostCase TNothing m = fmap ($ ()) m
hostCase (TJust (HostRev s)) m = RouteHost $ defaultingMap $ (\g -> fmap (. g) m) <$> singletonSequence s

pathCase :: TMaybe Path p -> RouteMap (h -> FromMaybeUnit p -> a) -> RouteMap (h -> a)
pathCase TNothing m = fmap (\f h -> f h ()) m
pathCase (TJust (Path s)) m = RoutePath $ defaultingMap $ (\g -> fmap (\f p h -> f h $ g p) m) <$> singletonSequence s

routeCase :: Route h m p a -> RouteCase a
routeCase Route{..} =
  hostCase routeHost $
  (if isNothing routeSecure then id else RouteSecure . singletonBool routeSecure) $
  pathCase routePath $
  when id (\m -> RouteMethod . defaultingMap . MonoidMap . Map.singleton m) routeMethod $
  (if routePriority == 0 then id else RoutePriority . Prioritized routePriority) $
  RouteExactly $ Exactly $ routeAction

routes :: [RouteCase a] -> RouteMap a
routes = mconcat

fallbackHEADtoGET :: RouteMap a -> RouteMap a
fallbackHEADtoGET (RouteMethod m) = RouteMethod $ fallbackDefaultMethodHEADtoGET m
fallbackHEADtoGET m = mapRoutes fallbackHEADtoGET m

unionSorted :: Ord a => [a] -> [a] -> [a]
unionSorted al@(a:ar) bl@(b:br) = case compare a b of
  LT -> a:unionSorted ar bl
  EQ -> a:unionSorted ar br
  GT -> b:unionSorted al br
unionSorted [] l = l
unionSorted l [] = l

data RouteResult a
  = RouteNotFound
  | AllowedMethods [Method]
  | RouteResult a
  | MultipleRoutes
  deriving (Functor)

instance Monoid (RouteResult a) where
  mempty = RouteNotFound
  mappend RouteNotFound r = r
  mappend (AllowedMethods _) r@(RouteResult _) = r
  mappend (AllowedMethods a) (AllowedMethods b) = AllowedMethods $ unionSorted a b
  mappend r@(RouteResult _) (AllowedMethods _) = r
  mappend MultipleRoutes _ = MultipleRoutes
  mappend r RouteNotFound = r
  mappend _ _ = MultipleRoutes

lookupRoute :: Request -> RouteMap a -> RouteResult a
lookupRoute Request{..} = rr where
  rr :: RouteMap a -> RouteResult a
  rr (RouteExactly Blank) = RouteNotFound
  rr (RouteExactly (Exactly a)) = RouteResult a
  rr (RouteExactly Conflict) = MultipleRoutes
  rr (RoutePriority (Prioritized _ r)) = rr r
  rr (RouteMethod m) = either AllowedMethods rr
    $ lookupDefaultMethod requestMethod m
  rr (RoutePath m) = seqr requestPath m
  rr (RouteSecure m) = mayber
    $ lookupBool requestSecure m
  rr (RouteHost m) = seqr requestHost m
  mayber :: Maybe (RouteMap a) -> RouteResult a
  mayber = maybe RouteNotFound rr
  seqr :: RouteString s => [s] -> DefaultMap (SequenceMap s) (RouteMap (SequencePlaceholderValue -> a)) -> RouteResult a
  seqr k (DefaultMap m d) = case lookupSequence k m of
    [] -> ($ []) <$> mayber d
    l -> foldMap (\(x, rm) -> ($ x) <$> rr rm) l

routeResult :: Request -> RouteResult a -> Either (Status, ResponseHeaders) a
routeResult _ RouteNotFound = Left (notFound404, [])
routeResult _ (AllowedMethods m) = Left (methodNotAllowed405, [(hAllow, BSC.intercalate (BSC.singleton ',') $ map renderParameter m)])
routeResult _ (RouteResult a) = Right a
routeResult q MultipleRoutes = Left $ error $ "Conflict routing " ++ show q

routeRequest :: Request -> RouteMap a -> Either (Status, ResponseHeaders) a
routeRequest q = routeResult q . lookupRoute q
