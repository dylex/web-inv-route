{-# LANGUAGE GADTs, TupleSections #-}
module Web.Route.Invertible.Map.HRoute
  ( routeCase
  , module Web.Route.Invertible.Map.Route
  ) where

import Control.Arrow (first)
import Data.HList.HList (HList(..))
import qualified Data.Map.Strict as Map

import Web.Route.Invertible.Host
import Web.Route.Invertible.Path
import Web.Route.Invertible.Method
import Web.Route.Invertible.HRoute
import Web.Route.Invertible.Monoid.Exactly
import Web.Route.Invertible.Monoid.Prioritized
import Web.Route.Invertible.Map.Monoid
import Web.Route.Invertible.Map.Default
import Web.Route.Invertible.Map.Bool
import Web.Route.Invertible.Map.Sequence
import Web.Route.Invertible.Map.Route hiding (routeCase)

collectMethods :: Route l -> ([Method], Route l)
collectMethods (RouteMethod r m) = first (m :) $ collectMethods r
collectMethods r = ([], r)

routeCase :: Action l a -> RouteCase a
routeCase (Action Route                     a) = RouteMapExactly $ Exactly $ a HNil
routeCase (Action (RouteHost r (HostRev s)) a) = RouteMapHost $ defaultingMap $ (\g -> fmap (. g) c) <$> singletonSequence s where
  c = routeCase $ Action r $ \l p -> a $ HCons p l
routeCase (Action (RouteSecure r s)         a) = RouteMapSecure $ singletonBool (Just s)
    $ routeCase $ Action r a
routeCase (Action (RoutePath r (Path s))    a) = RouteMapPath $ defaultingMap $ (\g -> fmap (. g) c) <$> singletonSequence s where
  c = routeCase $ Action r $ \l p -> a $ HCons p l
routeCase (Action (RouteMethod r m)         a) = RouteMapMethod $ defaultingMap $ MonoidMap $ Map.fromList $ map (, c) (m:ml) where
  (ml, r') = collectMethods r
  c = routeCase $ Action r' a
routeCase (Action (RoutePriority r p)       a) = RouteMapPriority $ Prioritized p
    $ routeCase $ Action r a
