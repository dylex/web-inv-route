-- |
-- This module exposes more of the inner workings of route construction for extensiblity and direct access.
-- You may need interfaces like 'IsMethod' or 'QueryParams' to add support for a new web framework, for example.
-- In particular, functions like 'foldRoute' and 'pathValues' can be used to extract low-level information about individual routes.
module Web.Route.Invertible.Internal
  ( -- * General
    RouteString(..)
  , Placeholder(..)
  , PlaceholderValue(..)
  , Sequence(..)
    -- * Path
  , Path(..)
  , normalizePath
  , pathValues
  , renderPath
  , urlPathBuilder
    -- * Host
  , Host(..)
  , splitHost
  , joinHost
  , renderHost
    -- * Method
  , IsMethod(..)
    -- * Query
  , QueryParams
  , paramsQuerySimple
  , simpleQueryParams
    -- * Route
  , blankRequest
  , RoutePredicate(..)
  , Route(..)
  , requestRoutePredicate
  , normRoute
  , foldRoute
  , requestRoute'
  , requestRoute
  ) where

import Web.Route.Invertible.String
import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Path
import Web.Route.Invertible.Method
import Web.Route.Invertible.Query
import Web.Route.Invertible.Request
import Web.Route.Invertible.Route
