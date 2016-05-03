-- |
-- An efficient routing map for host names.
module Web.Route.Invertible.Map.Host
  ( HostMap
  , HostPlaceholderValue
  , HostMapApp
  ) where

import Prelude hiding (lookup)

import Web.Route.Invertible.Host
import Web.Route.Invertible.Map.Sequence

type HostMap = SequenceMap HostString
type HostPlaceholderValue = SequencePlaceholderValue
type HostMapApp m a = SequenceMapApp HostString m a

