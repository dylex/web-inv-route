-- |Single-route construction.
-- This package lets you describe the individual end-points for routing and their associated values, essentially packaging up 'Host', 'Path', 'Method' and others with a value ('action') to represent an entry in your routing table.
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, DeriveDataTypeable, QuasiQuotes #-}
module Web.Route.Invertible.Route
  ( RoutePredicate(..)
  , Route(..)
  , routeHost
  , routeSecure
  , routePath
  , routeMethod
  , routePriority
  , normRoute
  , requestRoute'
  , requestRoute
  , Action(..)
  ) where

import Control.Arrow ((***), first, second)
import Control.Invertible.Monoidal
import Control.Invertible.Monoidal.Free
import Data.Function (on)
import qualified Data.Invertible as I
import Data.Monoid ((<>), Endo(..))

import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Host
import Web.Route.Invertible.Method
import Web.Route.Invertible.Path
import Web.Route.Invertible.Request

data RoutePredicate a where
  RouteHost     :: !(Host h) -> RoutePredicate h
  RouteSecure   :: !Bool     -> RoutePredicate ()
  RoutePath     :: !(Path p) -> RoutePredicate p
  RouteMethod   :: !Method   -> RoutePredicate ()
  RoutePriority :: !Int      -> RoutePredicate ()

instance Show (RoutePredicate a) where
  showsPrec d (RouteHost h) = showParen (d > 10) $
    showString "RouteHost " . showsPrec 11 h
  showsPrec d (RouteSecure s) = showParen (d > 10) $
    showString "RouteSecure " . showsPrec 11 s
  showsPrec d (RoutePath p) = showParen (d > 10) $
    showString "RoutePath " . showsPrec 11 p
  showsPrec d (RouteMethod m) = showParen (d > 10) $
    showString "RouteMethod " . showsPrec 11 m
  showsPrec d (RoutePriority p) = showParen (d > 10) $
    showString "RoutePriority " . showsPrec 11 p

newtype Route a = Route { freeRoute :: Free RoutePredicate a }
  deriving (I.Functor, Monoidal, MonoidalAlt)

instance Show (Route a) where
  showsPrec d (Route s) = showParen (d > 10) $
    showString "Route " . showsFree (showsPrec 11) s

routeHost :: Host h -> Route h
routeHost = Route . Free . RouteHost

routeSecure :: Bool -> Route ()
routeSecure = Route . Free . RouteSecure

routePath :: Path p -> Route p
routePath = Route . Free . RoutePath

routeMethod :: Method -> Route ()
routeMethod = Route . Free . RouteMethod

routePriority :: Int -> Route ()
routePriority = Route . Free . RoutePriority

predicateOrder :: RoutePredicate a -> Int
predicateOrder (RouteHost     _) = 1
predicateOrder (RouteSecure   _) = 2
predicateOrder (RoutePath     _) = 3
predicateOrder (RouteMethod   _) = 4
predicateOrder (RoutePriority _) = 5

data Range a
  = EmptyRange
  | Singleton !a
  | Range !a !a
  deriving (Eq)

rangeLower, rangeUpper :: Range a -> a
rangeLower (Range a _) = a
rangeLower (Singleton a) = a
rangeLower EmptyRange = error "rangeLower: EmptyRange"
rangeUpper (Range _ a) = a
rangeUpper (Singleton a) = a
rangeUpper EmptyRange = error "rangeUpper: EmptyRange"

instance Functor Range where
  fmap _ EmptyRange = EmptyRange
  fmap f (Singleton a) = Singleton (f a)
  fmap f (Range a b) = Range (f a) (f b)

instance Applicative Range where
  pure i = Range i i
  EmptyRange <*> _ = EmptyRange
  _ <*> EmptyRange = EmptyRange
  Singleton f <*> Singleton a = Singleton (f a)
  f <*> a = Range (rangeLower f $ rangeLower a) (rangeUpper f $ rangeUpper a)

instance Ord a => Monoid (Range a) where
  mempty = EmptyRange
  mappend r EmptyRange = r
  mappend EmptyRange r = r
  mappend (Singleton a) (Singleton b) = case compare a b of
    LT -> Range a b
    EQ -> Singleton a
    GT -> Range b a
  mappend a b = Range (on min rangeLower a b) (on max rangeUpper a b)

-- |Determine how two ranges should be sorted: Just for a definite ordering, or Nothing if overlapping
compareRange :: Ord a => Range a -> Range a -> Maybe Ordering
compareRange EmptyRange _ = Just EQ -- either way
compareRange _ EmptyRange = Just EQ -- either way
compareRange (Singleton a) (Singleton b) = Just $ compare a b
compareRange a b
  | rangeUpper a <= rangeLower b = Just LT -- definitely before
  | rangeLower a >= rangeUpper b = Just GT -- definitely after
  | otherwise = Nothing -- indeterminate

normJoin :: Free RoutePredicate a -> Free RoutePredicate b -> Free RoutePredicate (a, b)
normJoin (Transform f p) (Transform g q) = (f *** g) >$< normJoin p q
normJoin (Transform f p) q = first f >$< normJoin p q
normJoin p (Transform f q) = second f >$< normJoin p q
normJoin Empty (Join b c) = I.invert I.snd >$< normJoin b c
normJoin p@(Free x) (Join q@(Free y) r) | predicateOrder x > predicateOrder y =
  [I.biCase|(a,(b,c)) <-> (b,(a,c))|] >$< Join q (normJoin p r)
normJoin (Join p q) r = [I.biCase|(a,(b,c)) <-> ((a,b),c)|] >$< normJoin p (normJoin q r)
normJoin a b = Join a b -- shouldn't happen

normFree :: Free RoutePredicate a -> (Free RoutePredicate a, Range Int)
normFree Empty = (Empty, EmptyRange)
normFree (Transform f p) = first (Transform f) $ normFree p
normFree (Choose a b) = (Choose a' b', ar <> br) where
  (a', ar) = normFree a
  (b', br) = normFree b
normFree (Join a b) = (nj $ compareRange ar br, ar <> br) where
  nj (Just GT) = I.swap >$< Join b' a'
  nj (Just _) = Join a' b'
  nj Nothing = normJoin a' b'
  (a', ar) = normFree a
  (b', br) = normFree b
normFree f@(Free p) = (f, pure $ predicateOrder p)

normRoute :: Route a -> Route a
normRoute = Route . fst . normFree . freeTDNF . freeRoute

requestRoutePredicate :: RoutePredicate a -> a -> Request -> Request
requestRoutePredicate (RouteHost (HostRev s)) h q = q{ requestHost = renderSequence s h }
requestRoutePredicate (RouteSecure s)        () q = q{ requestSecure = s }
requestRoutePredicate (RoutePath (Path s))    p q = q{ requestPath = renderSequence s p }
requestRoutePredicate (RouteMethod m)        () q = q{ requestMethod = m }
requestRoutePredicate (RoutePriority _)      () q = q

requestRoute' :: Route a -> a -> Request -> Request
requestRoute' (Route r) = appEndo . foldFree (\p -> Endo . requestRoutePredicate p) r

requestRoute :: Route a -> a -> Request
requestRoute r a = requestRoute' r a blankRequest

data Action a b = Action
  { actionRoute :: !(Route a)
  , routeAction :: !(a -> b)
  }

infix 1 `Action`

instance Functor (Action a) where
  fmap f (Action r a) = Action r $ f . a
