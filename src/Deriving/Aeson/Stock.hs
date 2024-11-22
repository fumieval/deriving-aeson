{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Deriving.Aeson.Stock
  ( Prefixed
  , PrefixedSnake
  , Suffixed
  , SuffixedSnake
  , Snake
  , Vanilla
  -- * Reexports
  , CustomJSON(..)
  , FromJSON
  , ToJSON
  , Generic) where

import Data.Kind (Type)
import Deriving.Aeson

-- | Field names are prefixed by @str@; strip them from JSON representation
type Prefixed str = CustomJSON '[FieldLabelModifier (StripPrefix str)]

-- | Strip @str@ prefices and convert from CamelCase to snake_case
type PrefixedSnake str = CustomJSON '[FieldLabelModifier '[StripPrefix str, CamelToSnake]]

-- | Field names are suffixed by @str@; strip them from JSON representation
type Suffixed str = CustomJSON '[FieldLabelModifier (StripSuffix str)]

-- | Strip @str@ suffixes and convert from CamelCase to snake_case
type SuffixedSnake str = CustomJSON '[FieldLabelModifier '[StripSuffix str, CamelToSnake]]

-- | Convert from CamelCase to snake_case
type Snake = CustomJSON '[FieldLabelModifier CamelToSnake]

-- | No customisation
type Vanilla = CustomJSON ('[] :: [Type])
