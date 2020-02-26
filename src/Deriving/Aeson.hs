{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------
-- | Type-directed aeson instance CustomJSONisation
--------------------
module Deriving.Aeson
  ( CustomJSON(..)
  , FieldLabelModifier
  , ConstrctorTagModifier
  , OmitNothingFields
  , TagSingleConstructors
  , NoAllNullaryToStringTag
  -- * Name modifiers
  , StripPrefix
  , CamelToKebab
  , CamelToSnake
  -- * Interface
  , AesonOptions(..)
  , StringModifier(..)
  -- * Reexports
  , FromJSON
  , ToJSON
  , Generic
  )where

import Data.Aeson
import Data.Coerce
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

-- | A newtype wrapper which gives FromJSON/ToJSON instances with modified options.
newtype CustomJSON t a = CustomJSON { unCustomJSON :: a }

instance (AesonOptions t, Generic a, GFromJSON Zero (Rep a)) => FromJSON (CustomJSON t a) where
  parseJSON = (coerce `asTypeOf` fmap CustomJSON) . genericParseJSON (aesonOptions @t)
  {-# INLINE parseJSON #-}

instance (AesonOptions t, Generic a, GToJSON Zero (Rep a)) => ToJSON (CustomJSON t a) where
  toJSON = genericToJSON (aesonOptions @t) . unCustomJSON
  {-# INLINE toJSON #-}

-- | Function applied to field labels. Handy for removing common record prefixes for example.
data FieldLabelModifier t

-- | Function applied to constructor tags which could be handy for lower-casing them for example.
data ConstrctorTagModifier t

-- | Record fields with a Nothing value will be omitted from the resulting object.
data OmitNothingFields

-- | Encode types with a single constructor as sums, so that allNullaryToStringTag and sumEncoding apply.
data TagSingleConstructors

-- | the encoding will always follow the 'sumEncoding'.
data NoAllNullaryToStringTag

-- | Strip prefix @t@. If it doesn't have the prefix, keep it as-is.
data StripPrefix t

-- | CamelCase to snake_case
data CamelToSnake

-- | CamelCase to kebab-case
data CamelToKebab

-- | Reify a function which modifies names
class StringModifier t where
  getStringModifier :: String -> String

instance KnownSymbol k => StringModifier (StripPrefix k) where
  getStringModifier = fromMaybe <*> stripPrefix (symbolVal (Proxy @k))

-- | Left-to-right (@'flip' '.'@) composition
instance (StringModifier a, StringModifier b) => StringModifier (a, b) where
  getStringModifier = getStringModifier @b . getStringModifier @a

instance StringModifier CamelToKebab where
  getStringModifier = camelTo2 '-'

instance StringModifier CamelToSnake where
  getStringModifier = camelTo2 '_'

-- | Reify 'Options' from a type-level list
class AesonOptions xs where
  aesonOptions :: Options

instance AesonOptions '[] where
  aesonOptions = defaultOptions

instance AesonOptions xs => AesonOptions (OmitNothingFields ': xs) where
  aesonOptions = (aesonOptions @xs) { omitNothingFields = True }

instance (StringModifier f, AesonOptions xs) => AesonOptions (FieldLabelModifier f ': xs) where
  aesonOptions = (aesonOptions @xs) { fieldLabelModifier = getStringModifier @f }

instance (StringModifier f, AesonOptions xs) => AesonOptions (ConstrctorTagModifier f ': xs) where
  aesonOptions = (aesonOptions @xs) { constructorTagModifier = getStringModifier @f }

instance AesonOptions xs => AesonOptions (TagSingleConstructors ': xs) where
  aesonOptions = (aesonOptions @xs) { tagSingleConstructors = True }

instance AesonOptions xs => AesonOptions (NoAllNullaryToStringTag ': xs) where
  aesonOptions = (aesonOptions @xs) { allNullaryToStringTag = False }
