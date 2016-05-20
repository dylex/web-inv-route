-- |
-- An efficient routing map for host names.
module Web.Route.Invertible.Map.Host
  ( HostMap
  ) where

import Prelude hiding (lookup)

import Web.Route.Invertible.Host
import Web.Route.Invertible.Map.Sequence

-- |Map over 'Host' values.
type HostMap = SequenceMap HostString

