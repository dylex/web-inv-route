-- |Routing tables.
-- Once you have a number of endpoints defined, you can create a routing table of them using 'routeCase' and 'routes', dynamically combining the various Map representations as necessary to create a single, efficient map.
{-# LANGUAGE DeriveFunctor, RecordWildCards, GADTs, Rank2Types, TupleSections #-}
module Web.Route.Invertible.Map.Route
  ( RouteCase
  , RouteMap
  , routeCase
  , routeNormCase
  , routes
  , fallbackHEADtoGET
  , lookupRoute
  ) where

import Prelude hiding (lookup)

import Control.Applicative (Alternative(..))
import Control.Invertible.Monoidal.Free
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (evalState)
import Data.Dynamic (Dynamic, toDyn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

import Web.Route.Invertible.String
import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Path
import Web.Route.Invertible.Host
import Web.Route.Invertible.ContentType
import Web.Route.Invertible.Dynamics
import Web.Route.Invertible.Route
import Web.Route.Invertible.Request
import Web.Route.Invertible.Result
import Web.Route.Invertible.Monoid.Exactly
import Web.Route.Invertible.Monoid.Prioritized
import Web.Route.Invertible.Map.Monoid
import Web.Route.Invertible.Map.MonoidHash
import Web.Route.Invertible.Map.Default
import Web.Route.Invertible.Map.Const
import Web.Route.Invertible.Map.Bool
import Web.Route.Invertible.Map.Sequence
import Web.Route.Invertible.Map.Path
import Web.Route.Invertible.Map.Host
import Web.Route.Invertible.Map.Method
import Web.Route.Invertible.Map.Query
import Web.Route.Invertible.Map.Custom

-- |A routing table mapping 'Request's to values (actions) @a@.
data RouteMapT m a
  -- These constructors are expected to be nested in order for normalized routes
  = RouteMapHost      !(DefaultMap HostMap (RouteMapT m a))
  | RouteMapSecure    !(BoolMap (RouteMapT m a))
  | RouteMapPath      !(DefaultMap PathMap (RouteMapT m a))
  | RouteMapMethod    !(DefaultMap MethodMap (RouteMapT m a))
  | RouteMapQuery     !(QueryMap (RouteMapT m a))
  | RouteMapAccept    !(DefaultMap (MonoidHashMap ContentType) (RouteMapT m a))
  | RouteMapCustom    !(ConstMap (CustomMap Request Dynamic) (RouteMapT m a))
  | RouteMapPriority  !(Prioritized (RouteMapT m a))
  | RouteMapExactly   !(Exactly (m a))
  deriving (Show)

instance Semigroup (RouteMapT m a) where
  m <> RouteMapExactly Blank = m
  RouteMapExactly Blank <> m = m
  RouteMapHost     a <> RouteMapHost     b = RouteMapHost     (a <> b)
  RouteMapSecure   a <> RouteMapSecure   b = RouteMapSecure   (a <> b)
  RouteMapPath     a <> RouteMapPath     b = RouteMapPath     (a <> b)
  RouteMapMethod   a <> RouteMapMethod   b = RouteMapMethod   (a <> b)
  RouteMapQuery    a <> RouteMapQuery    b = RouteMapQuery    (a <> b)
  RouteMapAccept   a <> RouteMapAccept   b = RouteMapAccept   (a <> b)
  RouteMapCustom   a <> RouteMapCustom   b = RouteMapCustom   (a <> b)
  RouteMapPriority a <> RouteMapPriority b = RouteMapPriority (a <> b)
  RouteMapExactly  a <> RouteMapExactly  b = RouteMapExactly  (a <> b)
  a@(RouteMapHost     _) <> b = a <> RouteMapHost (defaultingValue b)
  a <> b@(RouteMapHost     _) =      RouteMapHost (defaultingValue a) <> b
  a@(RouteMapSecure   _) <> b = a <> RouteMapSecure (singletonBool Nothing b)
  a <> b@(RouteMapSecure   _) =      RouteMapSecure (singletonBool Nothing a) <> b
  a@(RouteMapPath     _) <> b = a <> RouteMapPath   (defaultingValue b)
  a <> b@(RouteMapPath     _) =      RouteMapPath   (defaultingValue a) <> b
  a@(RouteMapMethod   _) <> b = a <> RouteMapMethod (defaultingValue b)
  a <> b@(RouteMapMethod   _) =      RouteMapMethod (defaultingValue a) <> b
  a@(RouteMapQuery    _) <> b = a <> RouteMapQuery  (defaultQueryMap b)
  a <> b@(RouteMapQuery    _) =      RouteMapQuery  (defaultQueryMap a) <> b
  a@(RouteMapAccept   _) <> b = a <> RouteMapAccept (defaultingValue b)
  a <> b@(RouteMapAccept   _) =      RouteMapAccept (defaultingValue a) <> b
  a@(RouteMapCustom   _) <> b = a <> RouteMapCustom (constantValue b)
  a <> b@(RouteMapCustom   _) =      RouteMapCustom (constantValue a) <> b
  a@(RouteMapPriority _) <> b = a <> RouteMapPriority (Prioritized 0 b)
  a <> b@(RouteMapPriority _) =      RouteMapPriority (Prioritized 0 a) <> b

instance Monoid (RouteMapT m a) where
  mempty = RouteMapExactly Blank
  mappend = (<>)

exactlyMap :: m a -> RouteMapT m a
exactlyMap = RouteMapExactly . Exactly

mapRoutes :: (m a -> RouteMapT n b) -> (RouteMapT m a -> RouteMapT n b) -> RouteMapT m a -> RouteMapT n b
mapRoutes _ f (RouteMapHost     m) = RouteMapHost     $ f <$> m
mapRoutes _ f (RouteMapSecure   m) = RouteMapSecure   $ f <$> m
mapRoutes _ f (RouteMapPath     m) = RouteMapPath     $ f <$> m
mapRoutes _ f (RouteMapMethod   m) = RouteMapMethod   $ f <$> m
mapRoutes _ f (RouteMapQuery    m) = RouteMapQuery    $ f <$> m
mapRoutes _ f (RouteMapAccept   m) = RouteMapAccept   $ f <$> m
mapRoutes _ f (RouteMapCustom   m) = RouteMapCustom   $ f <$> m
mapRoutes _ f (RouteMapPriority m) = RouteMapPriority $ f <$> m
mapRoutes f _ (RouteMapExactly (Exactly a)) = f a
mapRoutes _ _ (RouteMapExactly Blank) = RouteMapExactly Blank
mapRoutes _ _ (RouteMapExactly Conflict) = RouteMapExactly Conflict

mapTails :: (m a -> RouteMapT n b) -> RouteMapT m a -> RouteMapT n b
mapTails f = mapRoutes f (mapTails f)

mapRoute :: (m a -> n b) -> RouteMapT m a -> RouteMapT n b
mapRoute f = mapTails (exactlyMap . f)

instance Functor f => Functor (RouteMapT f) where
  fmap f = mapRoute (fmap f)

instance Applicative f => Applicative (RouteMapT f) where
  pure = exactlyMap . pure
  f <*> m = mapTails (\f' -> mapRoute (f' <*>) m) f
  f  *> m = mapTails (\f' -> mapRoute (f'  *>) m) f

instance Applicative f => Alternative (RouteMapT f) where
  empty = RouteMapExactly $ empty
  (<|>) = mappend

{-
instance MonadTrans RouteMapT where
  lift = exactlyMap
-}

type RouteState = RouteMapT DynamicState
-- |The type of a route map element created from a single 'Route'.
-- These may be combined into a final 'RouteMap'.
-- (Currently these are in fact the same representation, but this may change.)
type RouteCase = RouteMapT ((->) Dynamics)
-- |A map for efficiently looking up requests based on a set of individual route specifications.
type RouteMap = RouteCase

sequenceState :: RouteString s => Sequence s a -> DefaultMap (SequenceMap s) (RouteState a)
sequenceState s = defaultingMap $ RouteMapExactly . Exactly <$> singletonSequence s

predicateState :: RoutePredicate a -> RouteState a
predicateState (RouteHost (HostRev s)) = RouteMapHost $ sequenceState s
predicateState (RouteSecure s) = RouteMapSecure $ singletonBool (Just s) $ pure ()
predicateState (RoutePath (Path s)) = RouteMapPath $ sequenceState s
predicateState (RouteMethod m) = RouteMapMethod $ defaultingMap $ MonoidMap $ Map.singleton m $ pure ()
predicateState (RouteQuery n p) = RouteMapQuery $ RouteMapExactly . Exactly <$> singletonQueryState n p
predicateState (RouteAccept t) = RouteMapAccept $ defaultingMap $ MonoidHashMap $ HM.singleton t $ pure ()
predicateState (RouteCustom f _) = RouteMapCustom $ constantMap $ singletonCustom (fmap toDyn . f) $
  RouteMapExactly $ Exactly $ getDynamic
predicateState (RoutePriority p) = RouteMapPriority $ Prioritized p $ pure ()

routeState :: Route a -> RouteState a
routeState (Route r) = runFree $ mapFree predicateState r

-- |Convert a 'Route' and result generator to a single entry in the routing table.
routeCase :: RouteAction a b -> RouteCase b
routeCase (RouteAction r f) = mapRoute (\s -> f . evalState s) $ routeState r

-- |Combine 'routeCase' and 'normRoute'.
-- See the description of 'normRoute' for an explaination.
routeNormCase :: RouteAction a b -> RouteCase b
routeNormCase (RouteAction r f) = mapRoute (\s -> f . evalState s) $ routeState $ normRoute r

-- |Combine a list of routes to a single map.
routes :: [RouteCase a] -> RouteMap a
routes = mconcat

-- |Make any handler for a 'GET' method in the map also apply to 'HEAD' requests, provided there is not an existing handler.
-- A number of frameworks can automatically convert your @GET@ responses into @HEAD@ responses, so this is useful (if slightly wasteful) in those cases.
fallbackHEADtoGET :: RouteMap a -> RouteMap a
fallbackHEADtoGET (RouteMapMethod m) = RouteMapMethod $ fallbackDefaultMethodHEADtoGET $ fallbackHEADtoGET <$> m
fallbackHEADtoGET m = mapRoutes exactlyMap fallbackHEADtoGET m

-- |Lookup a value in a routing table based on a 'Request'.
-- This returns the action returned by the 'route' that can handle this request, wrapped in a 'RouteResult' in case of error.
lookupRoute :: Request -> RouteMap a -> RouteResult a
lookupRoute q@Request{..} = rr id where
  rr :: (Dynamics -> Dynamics) -> RouteMap a -> RouteResult a
  rr _ (RouteMapExactly Blank) = RouteNotFound
  rr p (RouteMapExactly (Exactly a)) = RouteResult $ a $ p []
  rr _ (RouteMapExactly Conflict) = MultipleRoutes
  rr p (RouteMapPriority (Prioritized _ r)) = rr p r
  rr p (RouteMapCustom (ConstMap m cm)) =
    foldMap (\(x, rm) -> rr (p . (x:)) rm) (lookupCustom q m) <> rr p cm
  rr p (RouteMapAccept m) = mayber p $ lookupDefault (HM.lookup requestContentType . monoidHashMap) m
  rr p (RouteMapQuery m) = dynr p $ lookupQuery requestQuery m
  rr p (RouteMapMethod m) = either AllowedMethods (rr p)
    $ lookupDefaultMethod requestMethod m
  rr p (RouteMapPath m) = seqr p requestPath m
  rr p (RouteMapSecure m) = mayber p
    $ lookupBool requestSecure m
  rr p (RouteMapHost m) = seqr p requestHost m
  mayber :: (Dynamics -> Dynamics) -> Maybe (RouteMap a) -> RouteResult a
  mayber p = maybe RouteNotFound (rr p)
  dynr :: (Dynamics -> Dynamics) -> [DynamicResult (RouteMap a)] -> RouteResult a
  dynr p = foldMap (\(x, rm) -> rr (p . (x ++)) rm)
  seqr :: RouteString s => (Dynamics -> Dynamics) -> [s] -> DefaultMap (SequenceMap s) (RouteMap a) -> RouteResult a
  seqr p k (DefaultMap m d) = case lookupSequence k m of
    [] -> mayber p d
    l -> dynr p l
