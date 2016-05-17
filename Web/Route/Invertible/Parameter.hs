-- |Representations of values that can serve as placeholders, being parsed from "Web.Route.Invertible.String" data.
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, ScopedTypeVariables, ExistentialQuantification, EmptyCase, DefaultSignatures, FunctionalDependencies #-}
module Web.Route.Invertible.Parameter 
  ( Parameter(..)
  , Parameterized(..)
  , param
  , ParameterType(..)
  , parameterTypeOf
  , parseParameterAs
  ) where

import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.Hashable (Hashable(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Proxy (Proxy(Proxy))
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Typeable (Typeable, typeRep)
import Data.Void (Void, absurd)
import Text.Read (readMaybe)

import Web.Route.Invertible.String

-- |A parameter value @a@ that can be parsed from or rendered into string data @s@.
-- @parseParameter@ must invert @renderParameter@:
--
--   * @parseParameter . renderParameter == Just@
--   
class (RouteString s, Typeable a) => Parameter s a where
  -- |Parse string data into a value.
  -- Often equivalent (and defaults) to 'readMaybe'.
  parseParameter :: s -> Maybe a
  -- |Render a value into a string.
  -- Often equivalent (and defaults) to 'show'.
  renderParameter :: a -> s

  default parseParameter :: Read a => s -> Maybe a
  parseParameter = readMaybe . toString
  default renderParameter :: Show a => a -> s
  renderParameter = fromString . show

instance {-# OVERLAPPABLE #-} (RouteString s) => Parameter s String where
  parseParameter = Just . toString
  renderParameter = fromString
instance Parameter T.Text T.Text where
  parseParameter = Just
  renderParameter = id
instance Parameter BS.ByteString BS.ByteString where
  parseParameter = Just
  renderParameter = id

instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Integer
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Int
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Int8
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Int16
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Int32
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Int64
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Word
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Word8
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Word16
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Word32
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Word64
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Float
instance {-# OVERLAPPABLE #-} RouteString s => Parameter s Double

readText :: T.Reader a -> T.Text -> Maybe a
readText = (.) $ either (const Nothing) (\(a, t) -> a <$ guard (T.null t))

instance Parameter T.Text Integer where parseParameter = readText (T.signed T.decimal)
instance Parameter T.Text Int     where parseParameter = readText (T.signed T.decimal)
instance Parameter T.Text Int8    where parseParameter = readText (T.signed T.decimal)
instance Parameter T.Text Int16   where parseParameter = readText (T.signed T.decimal)
instance Parameter T.Text Int32   where parseParameter = readText (T.signed T.decimal)
instance Parameter T.Text Int64   where parseParameter = readText (T.signed T.decimal)
instance Parameter T.Text Word    where parseParameter = readText T.decimal
instance Parameter T.Text Word8   where parseParameter = readText T.decimal
instance Parameter T.Text Word16  where parseParameter = readText T.decimal
instance Parameter T.Text Word32  where parseParameter = readText T.decimal
instance Parameter T.Text Word64  where parseParameter = readText T.decimal
instance Parameter T.Text Float   where parseParameter = readText T.rational
instance Parameter T.Text Double  where parseParameter = readText T.double

instance RouteString s => Parameter s Void where
  parseParameter _ = Nothing
  renderParameter = absurd


-- |Parsers 'p' that operate over string data 's', and so can parse placeholder 'Parameter' values.
class Parameterized s p | p -> s where
  -- |Create a parser for a parameter of type 'a'.
  parameter :: Parameter s a => p a

-- |Create a placeholder 'parameter' with the type of the argument, which is ignored.
param :: (Parameterized s p, Parameter s a) => a -> p a
param _ = parameter

-- |An existential representation of an instance of @'Parameter' s@, that functions similarly to 'Data.Typeable.TypeRep' but also provides witness to a concrete instance.
data ParameterType s = forall a . Parameter s a => ParameterType !(Proxy a)

instance Eq (ParameterType s) where
  ParameterType a == ParameterType b = typeRep a == typeRep b
instance Ord (ParameterType s) where
  ParameterType a `compare` ParameterType b = typeRep a `compare` typeRep b
instance Hashable (ParameterType s) where
  hashWithSalt s (ParameterType d) = hashWithSalt s (typeRep d)
instance Show (ParameterType s) where
  showsPrec d (ParameterType p) = showParen (d > 10) $
    showString "ParameterType " . showsPrec 11 (typeRep p)

-- |Similar to 'typeRep'.
parameterTypeOf :: forall s proxy a . Parameter s a => proxy a -> ParameterType s
parameterTypeOf _ = ParameterType (Proxy :: Proxy a)

-- |Constrain the type of 'parseParameter', ignoring the first parameter.
parseParameterAs :: forall s proxy a . Parameter s a => proxy a -> s -> Maybe a
parseParameterAs _ = parseParameter
