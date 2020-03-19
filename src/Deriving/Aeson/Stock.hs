{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Deriving.Aeson.Stock
  ( Prefixed
  , PrefixedSnake
  , Snake
  , Vanilla
  -- * Reexports
  , CustomJSON(..)
  , FromJSON
  , ToJSON
  , Generic) where

import Deriving.Aeson

-- | Field names are prefixed by @str@; strip them from JSON representation
type Prefixed str = CustomJSON '[FieldLabelModifier (StripPrefix str)]

-- | Strip @str@ prefixes and convert from CamelCase to snake_case
type PrefixedSnake str = CustomJSON '[FieldLabelModifier (StripPrefix str, CamelToSnake)]

-- | Convert from CamelCase to snake_case
type Snake = CustomJSON '[FieldLabelModifier CamelToSnake]

-- | No customisation
type Vanilla = CustomJSON '[]
