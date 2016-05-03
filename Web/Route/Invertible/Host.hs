-- |
-- Domainname parsers.
-- These can be used for virtual hosting or otherwise matching on hostnames.
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}
module Web.Route.Invertible.Host
  ( HostString
  , splitHost
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

type HostString = BS.ByteString

-- |Split (and reverse) a domainname on \".\" for use with 'Host'.
splitHost :: BS.ByteString -> [HostString]
splitHost = reverse . BSC.split '.'

-- |A hostname matcher, providing the same functionality as 'Sequence'.
-- This matches hostnames in reverse order (from TLD down), but the 'Monoidal' instance and 'splitHost' automatically deal with this for you.
newtype Host a = HostRev { hostSequence :: Sequence HostString a }
  deriving (I.Functor, MonoidalAlt, Parameterized HostString)

instance Monoidal Host where
  unit = HostRev SequenceEmpty
  HostRev p >*< HostRev q = HostRev $ SequenceTransform I.swap $ SequenceJoin q p

-- |Since domain components cannot contain \".\", @"foo.com"@ is equivalent to @"foo" *< "com"@ (unlike other 'Sequence's).
instance IsString (Host ()) where
  fromString s = HostRev $ mapI_ (SequencePlaceholder . PlaceholderFixed) $ splitHost $ fromString s

-- |Instantiate a host with a value and render it as a domainname.
renderHost :: Host a -> a -> BS.ByteString
renderHost (HostRev p) a = BS.intercalate (BSC.singleton '.') $ reverse $ renderSequence p a
