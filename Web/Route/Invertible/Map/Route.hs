-- |Routing tables.
-- Once you have a number of endpoints defined, you can create a routing table of them using 'routeCase' and 'routes', dynamically combining the various Map representations as necessary to create a single, efficient map.
{-# LANGUAGE DeriveFunctor, RecordWildCards, GADTs, Rank2Types, TupleSections #-}
module Web.Route.Invertible.Map.Route
  ( RouteCase
  , RouteMap(..)
  , routeCase
  , routes
  , fallbackHEADtoGET
  , RouteResult(..)
  , lookupRoute
  ) where

import Prelude hiding (lookup)

import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)

import Web.Route.Invertible.String
import Web.Route.Invertible.Path
import Web.Route.Invertible.Host
import Web.Route.Invertible.Route
import Web.Route.Invertible.Request
import Web.Route.Invertible.Type
import Web.Route.Invertible.Result
import Web.Route.Invertible.Map.Monoid
import Web.Route.Invertible.Map.Default
import Web.Route.Invertible.Map.Bool
import Web.Route.Invertible.Map.Sequence
import Web.Route.Invertible.Map.Path
import Web.Route.Invertible.Map.Host
import Web.Route.Invertible.Map.Method
import Web.Route.Invertible.Monoid.Exactly
import Web.Route.Invertible.Monoid.Prioritized

-- |A routing table mapping 'Request's to values (actions) @a@.
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

hostCase :: TMaybe Host h -> RouteMap (FromMaybeVoid h -> a) -> RouteMap a
hostCase TNothing m = fmap ($ void) m
hostCase (TJust (HostRev s)) m = RouteHost $ defaultingMap $ (\g -> fmap (. g) m) <$> singletonSequence s

pathCase :: TMaybe Path p -> RouteMap (h -> FromMaybeVoid p -> a) -> RouteMap (h -> a)
pathCase TNothing m = fmap (\f h -> f h void) m
pathCase (TJust (Path s)) m = RoutePath $ defaultingMap $ (\g -> fmap (\f p h -> f h $ g p) m) <$> singletonSequence s

-- |Convert a single route end-point created by 'route' into a 'RouteCase', which abstracts over the various parameter types.
-- This converts a bi-directional route into its forward representation.
routeCase :: Route h p a -> RouteCase a
routeCase Route{..} =
  hostCase routeHost $
  (if isNothing routeSecure then id else RouteSecure . singletonBool routeSecure) $
  pathCase routePath $
  (if null routeMethod then id else \r -> RouteMethod $ defaultingMap $ MonoidMap $ Map.fromList $ map (, r) routeMethod) $
  (if routePriority == 0 then id else RoutePriority . Prioritized routePriority) $
  RouteExactly $ Exactly $ routeAction

-- |Combine 'RouteCase's into a single 'RouteMap'.
routes :: [RouteCase a] -> RouteMap a
routes = mconcat

-- |Make any handler for a 'GET' method in the map also apply to 'HEAD' requests, provided there is not an existing handler.
-- A number of frameworks can automatically convert your @GET@ responses into @HEAD@ responses, so this is useful (if slightly wasteful) in those cases.
fallbackHEADtoGET :: RouteMap a -> RouteMap a
fallbackHEADtoGET (RouteMethod m) = RouteMethod $ fallbackDefaultMethodHEADtoGET m
fallbackHEADtoGET m = mapRoutes fallbackHEADtoGET m

-- |Lookup a value in a routing table based on a 'Request'.
-- This returns the action returned by the 'route' that can handle this request, wrapped in a 'RouteResult' in case of error.
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
