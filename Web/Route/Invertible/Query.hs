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

type QueryString = BS.ByteString
type QueryParams = HM.HashMap QueryString [QueryString]

simpleQueryParams :: SimpleQuery -> QueryParams
simpleQueryParams = HM.fromListWith (++) . map (second return)

paramsQuerySimple :: QueryParams -> SimpleQuery
paramsQuerySimple q = [ (n, v) | (n, vl) <- HM.toList q, v <- vl ]
