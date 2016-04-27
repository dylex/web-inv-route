-- |
-- Single-route path parsers.
-- The most important type here is 'Path', which can be used to represent a single path endpoint within your application, including placeholders.
-- For example, the following represents a path of @\/item\/$id@ where @$id@ is an integer placeholder:
--
-- > Path ("item" *< parameter) :: Path Int
--
module Web.Route.Invertible.Path
  ( PathString
  , Path(..)
  , normalizePath
  , urlPathBuilder
  ) where

import Prelude hiding (lookup)

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types.URI as H

import Web.Route.Invertible.Sequence

type PathString = T.Text

newtype Path a = Path { pathSequence :: Sequence PathString a }

-- |Remove double- and trailing-slashes (i.e., empty path segments).
normalizePath :: [PathString] -> [PathString]
normalizePath = filter (not . T.null)

-- |Build a 'Path' as applied to a value into a bytestring 'B.Builder' by encoding the segments with 'urlEncodePath' and joining them with \"/\".
urlPathBuilder :: Path a -> a -> B.Builder
urlPathBuilder (Path p) a = foldMap es $ renderSequence p a where
  es s = B.char7 '/' <> H.urlEncodeBuilder False (TE.encodeUtf8 s)
