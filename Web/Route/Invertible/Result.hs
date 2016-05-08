-- |Representation of route table lookups.
module Web.Route.Invertible.Result
  ( RouteResult(..)
  , routeResult
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Typeable (Typeable)
import Network.HTTP.Types.Header (ResponseHeaders, hAllow)
import Network.HTTP.Types.Status (Status, notFound404, methodNotAllowed405)

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Method

data RouteResult a
  = RouteNotFound
  | AllowedMethods [Method]
  | RouteResult a
  | MultipleRoutes
  deriving (Eq, Show, Typeable)

instance Functor RouteResult where
  fmap _ RouteNotFound = RouteNotFound
  fmap _ (AllowedMethods m) = AllowedMethods m
  fmap f (RouteResult x) = RouteResult (f x)
  fmap _ MultipleRoutes = MultipleRoutes

instance Monoid (RouteResult a) where
  mempty = RouteNotFound
  mappend RouteNotFound r = r
  mappend (AllowedMethods _) r@(RouteResult _) = r
  mappend (AllowedMethods a) (AllowedMethods b) = AllowedMethods $ unionSorted a b
  mappend r@(RouteResult _) (AllowedMethods _) = r
  mappend MultipleRoutes _ = MultipleRoutes
  mappend r RouteNotFound = r
  mappend _ _ = MultipleRoutes

unionSorted :: Ord a => [a] -> [a] -> [a]
unionSorted al@(a:ar) bl@(b:br) = case compare a b of
  LT -> a:unionSorted ar bl
  EQ -> a:unionSorted ar br
  GT -> b:unionSorted al br
unionSorted [] l = l
unionSorted l [] = l

routeResult :: Show q => q -> RouteResult a -> Either (Status, ResponseHeaders) a
routeResult _ RouteNotFound = Left (notFound404, [])
routeResult _ (AllowedMethods m) = Left (methodNotAllowed405, [(hAllow, BSC.intercalate (BSC.singleton ',') $ map renderParameter m)])
routeResult _ (RouteResult a) = Right a
routeResult q MultipleRoutes = Left $ error $ "Conflict routing " ++ show q
