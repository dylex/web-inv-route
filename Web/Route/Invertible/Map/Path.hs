-- |
-- An efficient routing map for URL paths.
module Web.Route.Invertible.Map.Path
  ( PathMap
  , PathPlaceholderValue
  , PathMapApp
  ) where

import Prelude hiding (lookup)

import Web.Route.Invertible.Path
import Web.Route.Invertible.Map.Sequence

type PathMap = SequenceMap PathString
type PathPlaceholderValue = SequencePlaceholderValue
type PathMapApp m a = SequenceMapApp PathString m a
