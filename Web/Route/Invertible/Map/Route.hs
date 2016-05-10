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
import Web.Route.Invertible.Monoid.Exactly
import Web.Route.Invertible.Monoid.Prioritized
import Web.Route.Invertible.Map.Monoid
import Web.Route.Invertible.Map.Default
import Web.Route.Invertible.Map.Bool
import Web.Route.Invertible.Map.Sequence
import Web.Route.Invertible.Map.Path
import Web.Route.Invertible.Map.Host
import Web.Route.Invertible.Map.Method

-- |A routing table mapping 'Request's to values (actions) @a@.
data RouteMap a
  -- These constructors are expected to be nested in reverse order only, but we'd need some fancy type nat constraints to enforce this, and nothing really breaks that badly if they're not
  = RouteMapExactly   !(Exactly a)
  | RouteMapPriority  !(Prioritized (RouteMap a))
  | RouteMapMethod    !(DefaultMap MethodMap (RouteMap a))
  | RouteMapPath      !(DefaultMap PathMap (RouteMap (PathPlaceholderValue -> a)))
  | RouteMapSecure    !(BoolMap (RouteMap a))
  | RouteMapHost      !(DefaultMap HostMap (RouteMap (HostPlaceholderValue -> a)))
  deriving (Functor)

mapRoutes :: (forall b . RouteMap b -> RouteMap b) -> RouteMap a -> RouteMap a
mapRoutes f (RouteMapHost     m) = RouteMapHost     $ fmap f m
mapRoutes f (RouteMapSecure   m) = RouteMapSecure   $ fmap f m
mapRoutes f (RouteMapPath     m) = RouteMapPath     $ fmap f m
mapRoutes f (RouteMapMethod   m) = RouteMapMethod   $ fmap f m
mapRoutes f (RouteMapPriority m) = RouteMapPriority $ fmap f m
mapRoutes _ r@(RouteMapExactly _) = r

instance Monoid (RouteMap a) where
  mempty = RouteMapExactly Blank
  mappend (RouteMapExactly  a) (RouteMapExactly  b) = RouteMapExactly  (mappend a b)
  mappend (RouteMapPriority a) (RouteMapPriority b) = RouteMapPriority (mappend a b)
  mappend (RouteMapMethod   a) (RouteMapMethod   b) = RouteMapMethod   (mappend a b)
  mappend (RouteMapPath     a) (RouteMapPath     b) = RouteMapPath     (mappend a b)
  mappend (RouteMapSecure   a) (RouteMapSecure   b) = RouteMapSecure   (mappend a b)
  mappend (RouteMapHost     a) (RouteMapHost     b) = RouteMapHost     (mappend a b)
  mappend   a@(RouteMapHost     _) b = mappend a (RouteMapHost (defaultingValue $ fmap const b))
  mappend a b@(RouteMapHost     _)   = mappend   (RouteMapHost (defaultingValue $ fmap const a)) b
  mappend   a@(RouteMapSecure   _) b = mappend a (RouteMapSecure (singletonBool Nothing b))
  mappend a b@(RouteMapSecure   _)   = mappend   (RouteMapSecure (singletonBool Nothing a)) b
  mappend   a@(RouteMapPath     _) b = mappend a (RouteMapPath   (defaultingValue $ fmap const b))
  mappend a b@(RouteMapPath     _)   = mappend   (RouteMapPath   (defaultingValue $ fmap const a)) b
  mappend   a@(RouteMapMethod   _) b = mappend a (RouteMapMethod (defaultingValue b))
  mappend a b@(RouteMapMethod   _)   = mappend   (RouteMapMethod (defaultingValue a)) b
  mappend   a@(RouteMapPriority _) b = mappend a (RouteMapPriority (Prioritized 0 b)) 
  mappend a b@(RouteMapPriority _)   = mappend   (RouteMapPriority (Prioritized 0 a)) b 

type RouteCase = RouteMap

hostCase :: TMaybe Host h -> RouteMap (FromMaybeVoid h -> a) -> RouteMap a
hostCase TNothing m = fmap ($ void) m
hostCase (TJust (HostRev s)) m = RouteMapHost $ defaultingMap $ (\g -> fmap (. g) m) <$> singletonSequence s

pathCase :: TMaybe Path p -> RouteMap (h -> FromMaybeVoid p -> a) -> RouteMap (h -> a)
pathCase TNothing m = fmap (\f h -> f h void) m
pathCase (TJust (Path s)) m = RouteMapPath $ defaultingMap $ (\g -> fmap (\f p h -> f h $ g p) m) <$> singletonSequence s

-- |Convert a single route end-point created by 'route' into a 'RouteCase', which abstracts over the various parameter types.
-- This converts a bi-directional route into its forward representation.
routeCase :: Route h p a -> RouteCase a
routeCase Route{..} =
  hostCase routeHost $
  (if isNothing routeSecure then id else RouteMapSecure . singletonBool routeSecure) $
  pathCase routePath $
  (if null routeMethod then id else \r -> RouteMapMethod $ defaultingMap $ MonoidMap $ Map.fromList $ map (, r) routeMethod) $
  (if routePriority == 0 then id else RouteMapPriority . Prioritized routePriority) $
  RouteMapExactly $ Exactly $ routeAction

-- |Combine 'RouteCase's into a single 'RouteMap'.
routes :: [RouteCase a] -> RouteMap a
routes = mconcat

-- |Make any handler for a 'GET' method in the map also apply to 'HEAD' requests, provided there is not an existing handler.
-- A number of frameworks can automatically convert your @GET@ responses into @HEAD@ responses, so this is useful (if slightly wasteful) in those cases.
fallbackHEADtoGET :: RouteMap a -> RouteMap a
fallbackHEADtoGET (RouteMapMethod m) = RouteMapMethod $ fallbackDefaultMethodHEADtoGET m
fallbackHEADtoGET m = mapRoutes fallbackHEADtoGET m

-- |Lookup a value in a routing table based on a 'Request'.
-- This returns the action returned by the 'route' that can handle this request, wrapped in a 'RouteResult' in case of error.
lookupRoute :: Request -> RouteMap a -> RouteResult a
lookupRoute Request{..} = rr where
  rr :: RouteMap a -> RouteResult a
  rr (RouteMapExactly Blank) = RouteNotFound
  rr (RouteMapExactly (Exactly a)) = RouteResult a
  rr (RouteMapExactly Conflict) = MultipleRoutes
  rr (RouteMapPriority (Prioritized _ r)) = rr r
  rr (RouteMapMethod m) = either AllowedMethods rr
    $ lookupDefaultMethod requestMethod m
  rr (RouteMapPath m) = seqr requestPath m
  rr (RouteMapSecure m) = mayber
    $ lookupBool requestSecure m
  rr (RouteMapHost m) = seqr requestHost m
  mayber :: Maybe (RouteMap a) -> RouteResult a
  mayber = maybe RouteNotFound rr
  seqr :: RouteString s => [s] -> DefaultMap (SequenceMap s) (RouteMap (SequencePlaceholderValue -> a)) -> RouteResult a
  seqr k (DefaultMap m d) = case lookupSequence k m of
    [] -> ($ []) <$> mayber d
    l -> foldMap (\(x, rm) -> ($ x) <$> rr rm) l
