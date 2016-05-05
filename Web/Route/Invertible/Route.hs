{-# LANGUAGE FlexibleInstances, DataKinds, KindSignatures, RecordWildCards, MultiParamTypeClasses, FunctionalDependencies #-}
module Web.Route.Invertible.Route
  ( Route(..)
  , route
  , forHost
  , forSecure
  , forMethod
  , forPath
  , withPriority
  , action
  , requestRoute'
  , requestRoute
  ) where

import Data.Maybe (fromMaybe)

import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path
import Web.Route.Invertible.Request
import Web.Route.Invertible.Type

data Route (h :: Maybe *) (p :: Maybe *) (m :: Bool) a = Route
  { routeHost :: !(TMaybe Host h)
  , routeSecure :: !(Maybe Bool)
  , routePath :: !(TMaybe Path p)
  , routeMethod :: !(When m Method)
  , routePriority :: !Int
  , routeAction :: FromMaybeUnit h -> FromMaybeUnit p -> a
  }

instance Functor (Route h p m) where
  fmap f Route{ routeAction = a, .. } = 
    Route{ routeAction = \h p -> f $ a h p, .. }

route :: Route 'Nothing 'Nothing 'False ()
route = Route TNothing Nothing TNothing WhenNot 0 $ \_ _ -> ()

forHost :: Route 'Nothing p m a -> Host h -> Route ('Just h) p m a
forHost Route{ routeAction = f, .. } h =
  Route{ routeHost = TJust h, routeAction = \_ -> f (), .. }

forSecure :: Route h p m a -> Bool -> Route h p m a 
forSecure r s = r{ routeSecure = Just s }

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

requestRoute' :: Route h p m a -> FromMaybeUnit h -> FromMaybeUnit p -> Request -> Request
requestRoute' Route{..} h p Request{..} = Request
  { requestHost = tmaybe requestHost (\(HostRev s) -> renderSequence s h) routeHost
  , requestSecure = fromMaybe requestSecure routeSecure
  , requestPath = tmaybe requestPath (\(Path s) -> renderSequence s p) routePath
  , requestMethod = when requestMethod id routeMethod
  }

requestRoute :: Route h p m a -> FromMaybeUnit h -> FromMaybeUnit p -> Request
requestRoute r h p = requestRoute' r h p blankRequest

infixl 0 `forHost`, `forSecure`, `forPath`, `forMethod`, `withPriority`, `action`
