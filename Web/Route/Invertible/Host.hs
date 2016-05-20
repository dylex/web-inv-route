-- |
-- Domainname parsers (specialization of "Web.Route.Invertible.Sequence").
-- These can be used for virtual hosting or otherwise matching on hostnames.
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}
module Web.Route.Invertible.Host
  ( HostString
  , splitHost
  , joinHost
  , Host(..)
  , renderHost
  ) where

import Prelude hiding (lookup)

import Control.Invertible.Monoidal
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Invertible as I
import Data.String (IsString(..))

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Sequence

-- |The representation for domain names or domain name segments (after 'splitHost').
type HostString = BS.ByteString

-- |Split (and reverse) a domainname on \".\" for use with 'Host'.
splitHost :: BS.ByteString -> [HostString]
splitHost = reverse . BSC.split '.'

-- |Reverse and join a hostname with \".\".
joinHost :: [HostString] -> BS.ByteString
joinHost = BS.intercalate (BSC.singleton '.') . reverse

-- |A hostname matcher, providing the same functionality as 'Sequence'.
-- These should typically be constructed using the 'IsString' and 'Parameterized' instances.
-- This matches hostnames in reverse order (from TLD down), but the 'Monoidal' instance and 'splitHost' automatically deal with this for you.
-- Example:
--
-- > parameter >* "domain" >* "com" :: Host String
--
-- matches (or generates) @*.domain.com@ and returns the @*@ component.
newtype Host a = HostRev { hostSequence :: Sequence HostString a }
  deriving (I.Functor, MonoidalAlt, Parameterized HostString, Show)

instance Monoidal Host where
  unit = HostRev unit
  HostRev p >*< HostRev q = I.swap >$< HostRev (q >*< p)

-- |Since domain components cannot contain \".\", @"foo.com"@ is equivalent to @"foo" *< "com"@ (unlike other 'Sequence's).
instance IsString (Host ()) where
  fromString s = HostRev $ mapI_ (placeholderSequence . PlaceholderFixed) $ splitHost $ fromString s

-- |Instantiate a host with a value and render it as a domainname.
renderHost :: Host a -> a -> BS.ByteString
renderHost (HostRev p) a = joinHost $ renderSequence p a
