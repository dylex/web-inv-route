module Web.Route.Invertible.Route
  ( Route(..)
  ) where

import Web.Route.Invertible.Method
import Web.Route.Invertible.Path

data Route p a = Route
  { routeMethod :: Method
  , routePath :: Path p
  , routeAction :: p -> a
  }
