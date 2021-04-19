-- |Render requests in various ways
module Web.Route.Invertible.Render
  ( renderRequestBuilder
  , renderUrlRequestBuilder
  , renderHamletUrl
  ) where

import           Control.Arrow (second)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Types.URI (Query, encodePathSegments, renderQueryBuilder, simpleQueryToQuery, queryTextToQuery)

import Web.Route.Invertible.Request
import Web.Route.Invertible.Query
import Web.Route.Invertible.Route

-- |This renders a request along with additional query parameters as @[//host][/path][?query]@.
-- Each component is included only if it's specified by the route.
renderRequestBuilder :: Request -> Query -> B.Builder
renderRequestBuilder r q =
     bh (requestHost r)
  <> bp (requestPath r)
  <> renderQueryBuilder True ((simpleQueryToQuery $ paramsQuerySimple $ requestQuery r) ++ q)
  where
  bh [] = mempty
  bh [x] = sl <> sl <> B.byteString x
  bh (x:l) = bh l <> B.char8 '.' <> B.byteString x
  bp [] = sl
  bp p = encodePathSegments p
  sl = B.char8 '/'

-- |This renders a request along with additional query parameters as @scheme:[//host][/path][?query]@.
-- It's roughly equivalent to rendering the 'Web.Route.Invertible.URI.requestURI'.
renderUrlRequestBuilder :: Request -> Query -> B.Builder
renderUrlRequestBuilder r q =
  B.string8 (if requestSecure r then "https:" else "http:") <> renderRequestBuilder r q

-- |A 'Text.Hamlet.Render' function, suitable for passing to a @'Text.Hamlet.HtmlUrl' (RouteAction a b, a)@ template.
renderHamletUrl :: BoundRoute -> [(T.Text, T.Text)] -> T.Text
renderHamletUrl (r :? a) q = TE.decodeUtf8 $ BSL.toStrict $ B.toLazyByteString
  $ renderRequestBuilder (requestRoute r a) $ queryTextToQuery $ map (second Just) q
