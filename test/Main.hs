{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T
import System.Exit (exitSuccess, exitFailure)
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Test (isSuccess)

import Web.Route.Invertible
import Web.Route.Invertible.Request
import Web.Route.Invertible.Map.Route

getThing :: PathRoute Int String
getThing = route `forMethod` GET `forPath` "thing" *< parameter `action` \i ->
  "get thing" ++ show i

getThing2 :: PathRoute (Int, Int) String
getThing2 = route `forMethod` GET `forPath` "thing" *< parameter >*< parameter `action` \(i, sub) ->
  "get thing" ++ show i ++ "." ++ show sub

putThing :: PathRoute Int String
putThing = route `forMethod` PUT `forPath` "thing" *< parameter `action` \i ->
  "put thing" ++ show i

postThing :: PathRoute String String
postThing = route `forMethod` POST `forPath` "thing" *< parameter `action` \i ->
  "post thing=" ++ i

anyThingSub :: PathRoute (Int, [Int]) String
anyThingSub = route `forPath` "thing" *< parameter >* "sub" >*< manyI parameter `action` \(i, l) ->
  "thing" ++ show i ++ " sub" ++ concatMap ((' ' :) . show) l

ignoredThing :: PathRoute Int String
ignoredThing = route `forMethod` GET `forPath` "thing" *< parameter >* wildcard ["ign" :: String] `action` \i ->
  "ignore thing" ++ show i

things :: RouteMap String
things = routes
  [ routeCase getThing
  , routeCase putThing
  , routeCase postThing
  , routeCase getThing2
  , routeCase anyThingSub
  , routeCase ignoredThing
  ]

req :: Method -> [T.Text] -> Request
req m p = blankRequest
  { requestMethod = m
  , requestPath = p
  }

tests :: Q.Property
tests = Q.conjoin
  [ routeRequest (req PUT ["thing", "09"]) things Q.=== Right "put thing9"
  , requestRoute postThing "foo" Q.=== req POST ["thing", "foo"]
  , lookupRoute (req OPTIONS ["thing", "xxx"]) things Q.=== AllowedMethods [POST]
  , routeRequest (req PUT ["thing", "0", "sub", "12", "34"]) things Q.=== Right "thing0 sub 12 34"
  , routeRequest (req GET ["thing", "0", "12", "34"]) things Q.=== Right "ignore thing0"
  , requestRoute ignoredThing (-3) Q.=== req GET ["thing", "-3", "ign"]
  ]

main :: IO ()
main = do
  r <- Q.quickCheckResult tests
  if isSuccess r
    then exitSuccess
    else exitFailure
