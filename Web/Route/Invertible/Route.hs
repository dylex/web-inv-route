{-# LANGUAGE FlexibleInstances, DataKinds, KindSignatures, RecordWildCards, MultiParamTypeClasses, FunctionalDependencies #-}
module Web.Route.Invertible.Route
  ( Route(..)
  , route
  , forHost
  , forMethod
  , forPath
  , withPriority
  , action
  , requestRoute
  ) where

import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path
import Web.Route.Invertible.Request
import Web.Route.Invertible.Type

data Route (h :: Maybe *) (p :: Maybe *) (m :: Bool) a = Route
  { routeHost :: !(TMaybe Host h)
  , routePath :: !(TMaybe Path p)
  , routeMethod :: !(When m Method)
  , routePriority :: !Int
  , routeAction :: FromMaybeUnit h -> FromMaybeUnit p -> a
  }

instance Functor (Route h p m) where
  fmap f Route{ routeAction = a, .. } = 
    Route{ routeAction = \h p -> f $ a h p, .. }

route :: Route 'Nothing 'Nothing 'False ()
route = Route TNothing TNothing WhenNot 0 $ \_ _ -> ()

forHost :: Route 'Nothing p m a -> Host h -> Route ('Just h) p m a
forHost Route{ routeAction = f, .. } h =
  Route{ routeHost = TJust h, routeAction = \_ -> f (), .. }

forPath :: Route h 'Nothing m a -> Path p -> Route h ('Just p) m a
forPath Route{ routeAction = f, .. } p =
  Route{ routePath = TJust p, routeAction = \h _ -> f h (), .. }

forMethod :: IsMethod m => Route h p 'False a -> m -> Route h p 'True a
forMethod Route{ .. } m =
  Route{ routeMethod = WhenSo $ toMethod m, .. }

withPriority :: Route h p m a -> Int -> Route h p m a
withPriority r p = r{ routePriority = p }

class RouteAction r f a | r f -> a where
  action :: r () -> f -> r a

instance RouteAction (Route 'Nothing 'Nothing m) a a where
  action Route{ .. } f =
    Route{ routeAction = \_ _ -> f, .. }

instance RouteAction (Route ('Just h) 'Nothing m) (h -> a) a where
  action Route{ .. } f =
    Route{ routeAction = \h _ -> f h, .. }

instance RouteAction (Route 'Nothing ('Just p) m) (p -> a) a where
  action Route{ .. } f =
    Route{ routeAction = \_ -> f, .. }

instance RouteAction (Route ('Just h) ('Just p) m) (h -> p -> a) a where
  action Route{ .. } f =
    Route{ routeAction = f, .. }

requestRoute :: Route h p m a -> FromMaybeUnit h -> FromMaybeUnit p -> Request
requestRoute Route{..} h p = Request
  { requestHost = tmaybe [] (\(HostRev s) -> renderSequence s h) routeHost
  , requestPath = tmaybe [] (\(Path s) -> renderSequence s p) routePath
  , requestMethod = when (ExtensionMethod mempty) id routeMethod
  }

infixl 0 `forHost`, `forPath`, `forMethod`, `withPriority`, `action`
