{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BSB
import           Data.Maybe (fromJust)
import           Network.URI (parseURI)
import           System.Exit (exitSuccess, exitFailure)
import qualified Test.HUnit as U

import Web.Route.Invertible
import Web.Route.Invertible.URI
import Web.Route.Invertible.Render

getThing :: RouteAction Int String
getThing = routeMethod GET *< routePath ("thing" *< parameter) `RouteAction` \i ->
  "get thing" ++ show i

getThingQ :: RouteAction (Int, Char) String
getThingQ = routeMethod GET *< routePath ("thing" *< parameter) >*< routeQuery "type" parameter `RouteAction` \(i, c) ->
  "get thing" ++ show i ++ ":" ++ [c]

getThing2 :: RouteAction (Int, Int) String
getThing2 = routeMethod GET *< routePath ("thing" *< parameter >*< parameter) `RouteAction` \(i, sub) ->
  "get thing" ++ show i ++ "." ++ show sub

putThing :: RouteAction Int String
putThing = routeMethod PUT *< routePath ("thing" *< parameter) `RouteAction` \i ->
  "put thing" ++ show i

postThing :: RouteAction String String
postThing = routeMethod POST *< routePath ("thing" *< parameter) `RouteAction` \i ->
  "post thing=" ++ i

anyThingSub :: RouteAction (Int, [Int]) String
anyThingSub = routePath (("thing" *< parameter >* "sub") >*< manyI parameter) `RouteAction` \(i, l) ->
  "thing" ++ show i ++ " sub" ++ concatMap ((' ' :) . show) l

ignoredThing :: RouteAction Int String
ignoredThing = routeMethod GET *< routePath ("things" *< parameter >* wildcard ["ign" :: String]) `RouteAction` \i ->
  "ignore thing" ++ show i

complex :: RouteAction () String
complex = (routeMethod GET *< routeSecure False) *< (routePath "foo" *< routeHost ("foo" *< "com")) `RouteAction` \() ->
  "complex"

things :: RouteMap String
things = routes
  [ routeNormCase getThing
  , routeNormCase getThingQ
  , routeNormCase putThing
  , routeNormCase postThing
  , routeNormCase getThing2
  , routeNormCase anyThingSub
  , routeNormCase ignoredThing
  , routeNormCase complex
  ]

-- req :: Method -> String -> Request
-- req m = uriRequest m . fromJust . parseURI

rte :: Method -> String -> RouteResult String
rte m u = lookupRoute (uriRequest m $ fromJust $ parseURI u) things

req :: RouteAction r a -> r -> (Method, String)
req a = fmap show . routeActionURI a

ren :: RouteAction r a -> r -> BS.ByteString
ren r a = BSB.toLazyByteString $ renderUrlRequestBuilder (requestActionRoute r a) []

tests :: U.Test
tests = U.test
  [ rte GET "http:/thing/0"           U.~?= RouteResult "get thing0"
  , rte GET "http:/thing/-3?type=x"   U.~?= RouteResult "get thing-3:x"
  , req getThingQ (98, '=')           U.~?= (GET, "http:/thing/98?type=%3D")
  , ren getThingQ (98, '=')           U.~?= "http:/thing/98?type=%3D"
  , rte PUT "http:/thing/09"          U.~?= RouteResult "put thing9"
  , req postThing "foo"               U.~?= (POST, "http:/thing/foo")
  , ren postThing "foo"               U.~?= "http:/thing/foo"
  , rte OPTIONS "http:/thing/xxx"     U.~?= AllowedMethods [POST]
  , rte PUT "http:/thing/0/sub/12/34" U.~?= RouteResult "thing0 sub 12 34"
  , rte GET "http:/things/0/12/34"    U.~?= RouteResult "ignore thing0"
  , req ignoredThing (-3)             U.~?= (GET, "http:/things/-3/ign")
  , ren ignoredThing (-3)             U.~?= "http:/things/-3/ign"
  , rte POST "http://foo.com/foo"     U.~?= AllowedMethods [GET]
  , rte POST "http:/foo"              U.~?= RouteNotFound
  ]

main :: IO ()
main = do
  r <- U.runTestTT tests
  if U.errors r == 0 && U.failures r == 0
    then exitSuccess
    else exitFailure
