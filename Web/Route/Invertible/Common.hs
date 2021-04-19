-- |
-- The basic routing API, sufficient for route and routing map construction.
-- There is usually no need to import this module directly as it is re-exported by most other exposed interfaces.
--
-- This package supports route construction as follows:
--
--   1. Endpoint specification, which consists of a 'Route' /key/ and action /value/.
--
--       * Describe the parameters of each endpoint using 'Route', which is constructed by composing the @route*@ functions ('routeHost', 'routePath', 'routeMethod', 'routeQuery', etc.) using "Control.Invertible.Monoidal" operators.  Typically these predicates should be specified in order for sensible routing, but you can also use 'normRoute' to reorder them automatically.
--       * Join the route with a target using @\`RouteAction\`@.  This target action is the /value/ associated with the route /key/, and is usually an action to produce a response in your preferred framework (but could be anything).
--       * The 'RouteAction' should typically be assigned to a top-level variable.  While the type of the @Route@ parameter can differ for each route, all action values must have the same result type.
--
--       @
--       getThing :: 'RouteAction' Int (IO Response)
--       getThing =
--         'routePath' ("thing" *\< 'parameter') \>*
--         'routeMethod' GET
--         \`RouteAction\` \\thingId -> do
--           return Response{..}
--
--       postThingValue :: 'RouteAction' (Int, String) (IO Response)
--       postThingValue =
--         'routeSecure' True *\<
--         'routePath' ("thing" *\< 'parameter' \>* "value" \>*\< 'parameter') \>*
--         'routeMethod' POST
--         `RouteAction` \\(thingId, attribute) ->
--           set thingId attribute =<< getRequestBody
--           return Response{..}
--       @
--
--   2. Route Map specification.
--
--       * Apply each of the 'RouteAction's in your program to 'routeCase' (or 'routeNormCase' -- see 'normRoute').
--       * Apply 'routes' to a list of the resulting 'RouteCase's to form a 'RouteMap'.
--
--       @
--       myRoutes = 'routes'
--         [ 'routeCase' getThing
--         , 'routeCase' postThingValue
--         , ..
--         ]
--       @
--
--  You can also add support for new parameter types by creating your own instances of 'Parameter'.
module Web.Route.Invertible.Common
  ( 
    -- * Route construction
    Route
  , routeHost
  , routeSecure
  , routePath
  , routeMethod
  , routeMethods
  , routeQuery
  , routeAccept
  , routeAccepts
  , routeCustom
  , routePriority
  , BoundRoute(..)
  , RouteAction(..)
  , mapActionRoute
    -- ** Supporting types
  , Host
  , Path
  , IsMethod
  , QueryString
  , ContentType
    -- ** Parser construction
  , module Control.Invertible.Monoidal
  , wildcard
    -- ** Placeholder parameters
  , RouteString
  , Parameter(..)
  , Parameterized
  , parameter
  , param
  , Placeholder
    -- * Forward routing
  , RouteCase
  , routeCase
  , routeNormCase
  , RouteMap
  , routes
  , fallbackHEADtoGET
  ) where

import Control.Invertible.Monoidal

import Web.Route.Invertible.String
import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Path
import Web.Route.Invertible.Method
import Web.Route.Invertible.Query
import Web.Route.Invertible.ContentType
import Web.Route.Invertible.Route
import Web.Route.Invertible.Map.Route
