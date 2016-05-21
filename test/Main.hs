{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Maybe (fromJust)
import Network.URI (parseURI)
import System.Exit (exitSuccess, exitFailure)
import qualified Test.HUnit as U

import Web.Route.Invertible
import Web.Route.Invertible.URI

getThing :: Action Int String
getThing = routeMethod GET *< routePath ("thing" *< parameter) `Action` \i ->
  "get thing" ++ show i

getThingQ :: Action (Int, Char) String
getThingQ = routeMethod GET *< routePath ("thing" *< parameter) >*< routeQuery "type" parameter `Action` \(i, c) ->
  "get thing" ++ show i ++ ":" ++ [c]

getThing2 :: Action (Int, Int) String
getThing2 = routeMethod GET *< routePath ("thing" *< parameter >*< parameter) `Action` \(i, sub) ->
  "get thing" ++ show i ++ "." ++ show sub

putThing :: Action Int String
putThing = routeMethod PUT *< routePath ("thing" *< parameter) `Action` \i ->
  "put thing" ++ show i

postThing :: Action String String
postThing = routeMethod POST *< routePath ("thing" *< parameter) `Action` \i ->
  "post thing=" ++ i

anyThingSub :: Action (Int, [Int]) String
anyThingSub = routePath ("thing" *< parameter >* "sub" >*< manyI parameter) `Action` \(i, l) ->
  "thing" ++ show i ++ " sub" ++ concatMap ((' ' :) . show) l

ignoredThing :: Action Int String
ignoredThing = routeMethod GET *< routePath ("things" *< parameter >* wildcard ["ign" :: String]) `Action` \i ->
  "ignore thing" ++ show i

complex :: Action () String
complex = (routeMethod GET *< routeSecure False) *< (routePath "foo" *< routeHost ("foo" *< "com")) `Action` \() ->
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

req :: Action r a -> r -> (Method, String)
req a = fmap show . routeURI (actionRoute a)

tests :: U.Test
tests = U.test
  [ rte GET "http:/thing/0"           U.~?= RouteResult "get thing0"
  , rte GET "http:/thing/-3?type=x"   U.~?= RouteResult "get thing-3:x"
  , req getThingQ (98, '=')           U.~?= (GET, "http:/thing/98?type=%3D")
  , rte PUT "http:/thing/09"          U.~?= RouteResult "put thing9"
  , req postThing "foo"               U.~?= (POST, "http:/thing/foo")
  , rte OPTIONS "http:/thing/xxx"     U.~?= AllowedMethods [POST]
  , rte PUT "http:/thing/0/sub/12/34" U.~?= RouteResult "thing0 sub 12 34"
  , rte GET "http:/things/0/12/34"    U.~?= RouteResult "ignore thing0"
  , req ignoredThing (-3)             U.~?= (GET, "http:/things/-3/ign")
  , rte POST "http://foo.com/foo"     U.~?= AllowedMethods [GET]
  , rte POST "http:/foo"              U.~?= RouteNotFound
  ]

main :: IO ()
main = do
  r <- U.runTestTT tests
  if U.errors r == 0 && U.failures r == 0
    then exitSuccess
    else exitFailure
