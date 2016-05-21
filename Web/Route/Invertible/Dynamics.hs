-- |
-- As keys are looked up in maps, dynamic parsed placeholders are collected as a list of 'Dynamic's.
module Web.Route.Invertible.Dynamics
  ( Dynamics
  , DynamicState
  , DynamicResult
  , getDynamic
  ) where

import Data.Dynamic (Dynamic, fromDyn)
import Data.Typeable (Typeable)
import Control.Monad.Trans.State (StateT(..), State)

-- |Uncons the current state, leaving the tail in the state and returning the head.
-- (This should probably be moved to some other module.)
unconsState' :: State [a] a
unconsState' = StateT $ \(~(x:l)) -> return (x, l)

type Dynamics = [Dynamic]
type DynamicState = State Dynamics
type DynamicResult a = (Dynamics, a)

getDynamic :: Typeable a => DynamicState a
getDynamic = (`fromDyn` error "Web.Route.Invertible.getDynamic: internal type error") <$> unconsState'

