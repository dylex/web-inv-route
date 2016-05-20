-- |
-- An efficient routing map for URL paths.
module Web.Route.Invertible.Map.Path
  ( PathMap
  ) where

import Prelude hiding (lookup)

import Web.Route.Invertible.Path
import Web.Route.Invertible.Map.Sequence

-- |Map over 'Path' values.
type PathMap = SequenceMap PathString
