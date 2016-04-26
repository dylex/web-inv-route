module Web.Route.Invertible.Route
  ( Route(..)
  , action
  ) where

import Web.Route.Invertible.Method
import Web.Route.Invertible.Path

data Route p a = Route
  { routeMethod :: Method
  , routePath :: Path p
  , routeAction :: p -> a
  }

action :: IsMethod m => m -> Path p -> (p -> a) -> Route p a
action m p f = Route (toMethod m) p f
