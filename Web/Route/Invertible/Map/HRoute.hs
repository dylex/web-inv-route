{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TupleSections #-}
module Web.Route.Invertible.Map.HRoute
  ( routeCase
  , module Web.Route.Invertible.Map.Route
  ) where

import Control.Arrow (first)
import qualified Data.Map.Strict as Map

import Web.Route.Invertible.Host
import Web.Route.Invertible.Path
import Web.Route.Invertible.Method
import Web.Route.Invertible.Type
import Web.Route.Invertible.HRoute
import Web.Route.Invertible.Monoid.Exactly
import Web.Route.Invertible.Monoid.Prioritized
import Web.Route.Invertible.Map.Monoid
import Web.Route.Invertible.Map.Default
import Web.Route.Invertible.Map.Bool
import Web.Route.Invertible.Map.Sequence
import Web.Route.Invertible.Map.Path
import Web.Route.Invertible.Map.Host
import Web.Route.Invertible.Map.Method
-- import Web.Route.Invertible.Map.Route hiding (routeCase)

-- |A routing table mapping 'Request's to values (actions) @a@.
data RouteMap a
  -- These constructors are expected to be nested in reverse order only, but we'd need some fancy type nat constraints to enforce this, and nothing really breaks that badly if they're not
  = RouteMapExactly   !(Exactly ([[SequencePlaceholderValue]] -> a))
  | RouteMapPriority  !(Prioritized (RouteMap a))
  | RouteMapMethod    !(DefaultMap MethodMap (RouteMap a))
  | RouteMapPath      !(DefaultMap PathMap (RouteMap a))
  | RouteMapSecure    !(BoolMap (RouteMap a))
  | RouteMapHost      !(DefaultMap HostMap (RouteMap a))

collectMethods :: Route l -> ([Method], Route l)
collectMethods (RouteMethod m r) = first (m :) $ collectMethods r
collectMethods r = ([], r)

routeActionCase :: Route l -> Functions l a -> RouteMap a
routeActionCase Route                     a = RouteMapExactly $ Exactly $ const a
routeActionCase (RouteHost (HostRev s) r) a = RouteMapHost $ defaultingMap $ (\g -> routeActionCase r (a . g)) <$> singletonSequence s where
  c = routeActionCase r a
  
  -- a :: = h -> Functions l a
  -- singletonSequence s :: HostMap (HostPlaceholderValue -> h)
  -- routeActionCase r :: Functions l a -> RouteCase a
  -- need :: RouteCase (HostPlaceholderValue -> a)
routeActionCase (RouteSecure s         r) a = RouteMapSecure $ singletonBool (Just s)
    $ routeActionCase r a
routeActionCase (RoutePath (Path s)    r) a = RouteMapPath $ defaultingMap $ (\g -> fmap (. g) c) <$> singletonSequence s where
  c = routeActionCase r a
routeActionCase (RouteMethod m         r) a = RouteMapMethod $ defaultingMap $ MonoidMap $ Map.fromList $ map (, c) (m:ml) where
  (ml, r') = collectMethods r
  c = routeActionCase r' a
routeActionCase (RoutePriority p       r) a = RouteMapPriority $ Prioritized p
    $ routeActionCase r a

routeCase :: Action l a -> RouteMap '[] a
routeCase (Action r a) = routeActionCase r a

routeAnyCase :: SomeAction a -> RouteMap '[] a
routeAnyCase (SomeAction (Action r a)) = routeActionCase r a
