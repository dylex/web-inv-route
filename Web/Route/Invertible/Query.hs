module Web.Route.Invertible.Query
  ( QueryString
  , QueryParams
  , simpleQueryParams
  , paramsQuerySimple
  ) where

import Control.Arrow (second)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as HM
import Network.HTTP.Types.URI (SimpleQuery)

-- |The type of URL query strings, variables, and parameters, after URI decoding but before UTF-8 decoding.
type QueryString = BS.ByteString
-- |A map from query variables to values, based on 'SimpleQuery'.
type QueryParams = HM.HashMap QueryString [QueryString]

simpleQueryParams :: SimpleQuery -> QueryParams
simpleQueryParams = HM.fromListWith (++) . map (second return)

paramsQuerySimple :: QueryParams -> SimpleQuery
paramsQuerySimple q = [ (n, v) | (n, vl) <- HM.toList q, v <- vl ]
