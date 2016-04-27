{-# LANGUAGE FlexibleInstances #-}
module Web.Route.Invertible.Route
  ( Route(..)
  , action
  ) where

import Web.Route.Invertible.Method
import Web.Route.Invertible.Path

data Route p a = Route
  { routeMethod :: Maybe Method
  , routePath :: Path p
  , routeAction :: p -> a
  }

class RouteAction r where
  action :: r

instance RouteAction (Path p -> (p -> a) -> Route p a) where
  action p f = Route Nothing p f

instance IsMethod m => RouteAction (m -> Path p -> (p -> a) -> Route p a) where
  action m p f = Route (Just $ toMethod m) p f
