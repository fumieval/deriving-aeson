{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
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
  , ConstructorTagModifier
  , OmitNothingFields
  , RejectUnknownFields
  , TagSingleConstructors
  , NoAllNullaryToStringTag
  , UnwrapUnaryRecords
  -- * Sum encoding
  , SumTaggedObject
  , SumUntaggedValue
  , SumObjectWithSingleField
  , SumTwoElemArray
  -- * Name modifiers
  , StripPrefix
  , CamelTo
  , CamelToKebab
  , CamelToSnake
  , Rename
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
import Data.Kind
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

instance (AesonOptions t, Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (CustomJSON t a) where
  toJSON = genericToJSON (aesonOptions @t) . unCustomJSON
  {-# INLINE toJSON #-}
  toEncoding = genericToEncoding (aesonOptions @t) . unCustomJSON
  {-# INLINE toEncoding #-}

-- | Function applied to field labels. Handy for removing common record prefixes for example.
data FieldLabelModifier t

-- | Function applied to constructor tags which could be handy for lower-casing them for example.
data ConstructorTagModifier t

-- | Record fields with a Nothing value will be omitted from the resulting object.
data OmitNothingFields

-- | JSON Documents mapped to records with unmatched keys will be rejected
data RejectUnknownFields

-- | Encode types with a single constructor as sums, so that allNullaryToStringTag and sumEncoding apply.
data TagSingleConstructors

-- | the encoding will always follow the 'sumEncoding'.
data NoAllNullaryToStringTag

-- | Unpack single-field records
data UnwrapUnaryRecords

-- | Strip prefix @t@. If it doesn't have the prefix, keep it as-is.
data StripPrefix t

-- | Generic CamelTo constructor taking in a separator char
data CamelTo (separator :: Symbol)

-- | CamelCase to snake_case
type CamelToSnake = CamelTo "_"

-- | CamelCase to kebab-case
type CamelToKebab = CamelTo "-"

-- | Rename fields called @from@ to @to@.
data Rename (from :: Symbol) (to :: Symbol)

-- | Reify a function which modifies names
class StringModifier t where
  getStringModifier :: String -> String

instance KnownSymbol k => StringModifier (StripPrefix k) where
  getStringModifier = fromMaybe <*> stripPrefix (symbolVal (Proxy @k))

instance StringModifier '[] where
  getStringModifier = id

-- | Left-to-right (@'foldr' ('flip' ('.')) 'id'@) composition
instance (StringModifier a, StringModifier as) => StringModifier (a ': as) where
  getStringModifier = getStringModifier @as . getStringModifier @a

-- | Left-to-right (@'flip' '.'@) composition
instance (StringModifier a, StringModifier b) => StringModifier (a, b) where
  getStringModifier = getStringModifier @b . getStringModifier @a

-- | Left-to-right (@'flip' '.'@) composition
instance (StringModifier a, StringModifier b, StringModifier c) => StringModifier (a, b, c) where
  getStringModifier = getStringModifier @c . getStringModifier @b . getStringModifier @a

-- | Left-to-right (@'flip' '.'@) composition
instance (StringModifier a, StringModifier b, StringModifier c, StringModifier d) => StringModifier (a, b, c, d) where
  getStringModifier = getStringModifier @d . getStringModifier @c . getStringModifier @b . getStringModifier @a

instance (KnownSymbol separator, NonEmptyString separator) => StringModifier (CamelTo separator) where
  getStringModifier = camelTo2 char
    where
      char = case symbolVal (Proxy @separator) of
        c : _ -> c
        _ -> error "Impossible"

instance (KnownSymbol from, KnownSymbol to) => StringModifier (Rename from to) where
  getStringModifier s = if s == symbolVal (Proxy @from) then symbolVal (Proxy @to) else s

type family NonEmptyString (xs :: Symbol) :: Constraint where
  NonEmptyString "" = TypeError ('Text "Empty string separator provided for camelTo separator")
  NonEmptyString _  = ()

-- | @{ "tag": t, "content": c}@
data SumTaggedObject t c

-- | @CONTENT@
data SumUntaggedValue

-- | @{ TAG: CONTENT }@
data SumObjectWithSingleField

-- | @[TAG, CONTENT]@
data SumTwoElemArray

-- | Reify 'Options' from a type-level list
class AesonOptions xs where
  aesonOptions :: Options

instance AesonOptions '[] where
  aesonOptions = defaultOptions

instance AesonOptions xs => AesonOptions (UnwrapUnaryRecords ': xs) where
  aesonOptions = (aesonOptions @xs) { unwrapUnaryRecords = True }

instance AesonOptions xs => AesonOptions (OmitNothingFields ': xs) where
  aesonOptions = (aesonOptions @xs) { omitNothingFields = True }

instance AesonOptions xs => AesonOptions (RejectUnknownFields ': xs) where
  aesonOptions = (aesonOptions @xs) { rejectUnknownFields = True }

instance (StringModifier f, AesonOptions xs) => AesonOptions (FieldLabelModifier f ': xs) where
  aesonOptions = let next = aesonOptions @xs in
    next { fieldLabelModifier = fieldLabelModifier next . getStringModifier @f }

instance (StringModifier f, AesonOptions xs) => AesonOptions (ConstructorTagModifier f ': xs) where
  aesonOptions = let next = aesonOptions @xs in
    next { constructorTagModifier = constructorTagModifier next . getStringModifier @f }

instance AesonOptions xs => AesonOptions (TagSingleConstructors ': xs) where
  aesonOptions = (aesonOptions @xs) { tagSingleConstructors = True }

instance AesonOptions xs => AesonOptions (NoAllNullaryToStringTag ': xs) where
  aesonOptions = (aesonOptions @xs) { allNullaryToStringTag = False }

instance (KnownSymbol t, KnownSymbol c, AesonOptions xs) => AesonOptions (SumTaggedObject t c ': xs) where
  aesonOptions = (aesonOptions @xs) { sumEncoding = TaggedObject (symbolVal (Proxy @t)) (symbolVal (Proxy @c)) }

instance (AesonOptions xs) => AesonOptions (SumUntaggedValue ': xs) where
  aesonOptions = (aesonOptions @xs) { sumEncoding = UntaggedValue }

instance (AesonOptions xs) => AesonOptions (SumObjectWithSingleField ': xs) where
  aesonOptions = (aesonOptions @xs) { sumEncoding = ObjectWithSingleField }

instance (AesonOptions xs) => AesonOptions (SumTwoElemArray ': xs) where
  aesonOptions = (aesonOptions @xs) { sumEncoding = TwoElemArray }
