module Web.Route.Method
  ( MethodPathRoute(..)
  , MethodPathMap
  , singleton
  , lookup
  ) where

import Prelude hiding (lookup)

import qualified Control.Invariant.Functor as Inv
import qualified Data.Isomorphism as I
import Network.HTTP.Types.Method (StdMethod, Method)

import Web.Route.String
import Web.Route.Path (Path, PathRoute(..))
import qualified Web.Route.Path.Map as P
import qualified Web.Route.Method.Map as M
import Web.Route.Resolver

data MethodPathRoute s a b = MethodPathRoute
  { routeMethod :: StdMethod
  , routePath :: Path s b
  , pathRouteAction :: b -> a
  }

instance Inv.Functor (MethodPathRoute s b) where
  fmap f (MethodPathRoute m p g) = MethodPathRoute m (Inv.fmap f p) (g . I.isoFrom f)

type MethodPathMap s a = P.PathMap s (M.MethodMap (Exactly a))

singleton :: RouteString s => MethodPathRoute s a b -> MethodPathMap s a
singleton (MethodPathRoute m p f) = P.singleton $ PathRoute p $ M.singleton m . Exactly . f

lookup :: RouteString s => Method -> [s] -> MethodPathMap s a -> Either [Method] a
lookup m l p = maybe (Left []) Right . exactlyToMaybe =<< M.lookupMethod m (P.lookup l p)
