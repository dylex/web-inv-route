{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T
import System.Exit (exitSuccess, exitFailure)
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Test (isSuccess)

import Web.Route.Invertible
import Web.Route.Invertible.Request
import Web.Route.Invertible.Map.Route

getThing :: Action Int String
getThing = routeMethod GET *< routePath ("thing" *< parameter) `Action` \i ->
  "get thing" ++ show i

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
ignoredThing = routeMethod GET *< routePath ("thing" *< parameter >* wildcard ["ign" :: String]) `Action` \i ->
  "ignore thing" ++ show i

things :: RouteMap String
things = routes
  [ routeNormCase getThing
  , routeNormCase putThing
  , routeNormCase postThing
  , routeNormCase getThing2
  , routeNormCase anyThingSub
  , routeNormCase ignoredThing
  ]

req :: Method -> [T.Text] -> Request
req m p = blankRequest
  { requestMethod = m
  , requestPath = p
  }

tests :: Q.Property
tests = Q.conjoin
  [ routeRequest (req PUT ["thing", "09"]) things Q.=== Right "put thing9"
  , requestRoute (actionRoute postThing) "foo" Q.=== req POST ["thing", "foo"]
  , lookupRoute (req OPTIONS ["thing", "xxx"]) things Q.=== AllowedMethods [POST]
  , routeRequest (req PUT ["thing", "0", "sub", "12", "34"]) things Q.=== Right "thing0 sub 12 34"
  , routeRequest (req GET ["thing", "0", "12", "34"]) things Q.=== Right "ignore thing0"
  , requestRoute (actionRoute ignoredThing) (-3) Q.=== req GET ["thing", "-3", "ign"]
  ]

main :: IO ()
main = do
  r <- Q.quickCheckResult tests
  if isSuccess r
    then exitSuccess
    else exitFailure
