-- |
-- Single-route path parsers (specialization of "Web.Route.Invertible.Sequence").
-- The most important type here is 'Path', which can be used to represent a single path endpoint within your application, including placeholders.
-- For example, the following represents a path of @\/item\/$id@ where @$id@ is an integer placeholder:
--
-- > Path ("item" *< parameter) :: Path Int
--
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}
module Web.Route.Invertible.Path
  ( PathString
  , normalizePath
  , Path(..)
  , urlPathBuilder
  ) where

import Prelude hiding (lookup)

import Control.Invertible.Monoidal
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Invertible as I
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types.URI as H

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Sequence

-- |A component of a path, such that paths are represented by @['PathString']@ (after splitting on \'/\').
-- Paths can be created by 'H.decodePath'.
type PathString = T.Text

-- |Remove double- and trailing-slashes (i.e., empty path segments).
normalizePath :: [PathString] -> [PathString]
normalizePath = filter (not . T.null)

-- |A URL path parser/generator, providing the same functionality as 'Sequence'.
-- These should typically be constructed using the 'IsString' and 'Parameterized' instances.
-- Note that the individual components are /decoded/ path segments, so a literal slash in a component (e.g., as produced with 'fromString') will match \"%2F\".
-- Example:
--
-- > "get" *< parameter >*< "value" *< parameter :: Path (String, Int)
--
-- matches (or generates) @\/get\/$x\/value\/$y@ for any string @$x@ and any int @$y@ and returns those values.
newtype Path a = Path { pathSequence :: Sequence PathString a }
  deriving (I.Functor, Monoidal, MonoidalAlt, Parameterized PathString, Show)

deriving instance IsString (Path ())

-- |Build a 'Path' as applied to a value into a bytestring 'B.Builder' by encoding the segments with 'urlEncodePath' and joining them with \"/\".
urlPathBuilder :: Path a -> a -> B.Builder
urlPathBuilder (Path p) a = foldMap es $ renderSequence p a where
  es s = B.char7 '/' <> H.urlEncodeBuilder False (TE.encodeUtf8 s)
