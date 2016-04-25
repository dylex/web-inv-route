module Web.Route.Wai
  ( routePath
  , routeMethodPath
  ) where

import Control.Arrow (left)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Network.Wai as Wai
import Network.HTTP.Types.Header (ResponseHeaders, hAllow)
import Network.HTTP.Types.Status (Status, notFound404, methodNotAllowed405)

import qualified Web.Route.Path as P
import qualified Web.Route.Method as M

routePath :: Wai.Request -> P.PathMap T.Text a -> Either Status a
routePath q m = maybe (Left notFound404) Right $ P.lookup (Wai.pathInfo q) m

routeMethodPath :: Wai.Request -> M.MethodPathMap T.Text a -> Either (Status, ResponseHeaders) a
routeMethodPath q m = left err $ M.lookup (Wai.requestMethod q) (Wai.pathInfo q) m where
  err [] = (notFound404, [])
  err a = (methodNotAllowed405, [(hAllow, BS.intercalate (BS.singleton ',') a)])
