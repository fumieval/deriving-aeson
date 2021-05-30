deriving-aeson
====

[![Hackage](https://img.shields.io/hackage/v/deriving-aeson.svg)](https://hackage.haskell.org/package/deriving-aeson)
![Haskell CI](https://github.com/fumieval/deriving-aeson/workflows/Haskell%20CI/badge.svg)
[![Discord](https://img.shields.io/discord/664807830116892674?color=%237095ec&label=Discord&style=plastic)](https://discord.gg/DG93Tgs)

![logo](https://github.com/fumieval/deriving-aeson/blob/master/logo/logo.png?raw=true)

This package provides a newtype wrapper where you can customise
[aeson](https://hackage.haskell.org/package/aeson)'s generic methods using a
type-level interface, which synergises well with DerivingVia.

```haskell
{-# LANGUAGE DerivingVia, DataKinds, DeriveGeneric #-}
import Data.Aeson
import Deriving.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

data User = User
  { userId :: Int
  , userName :: String
  , userAPIToken :: Maybe String
  } deriving Generic
  deriving (FromJSON, ToJSON)
  via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "user", CamelToSnake]] User

testData :: [User]
testData = [User 42 "Alice" Nothing, User 43 "Bob" (Just "xyz")]

main = BL.putStrLn $ encode testData
-- [{"name":"Alice","id":42},{"api_token":"xyz","name":"Bob","id":43}]
```

`Deriving.Aeson.Stock` contains some aliases for even less boilerplates.

* `Prefixed str` = `CustomJSON '[FieldLabelModifier (StripPrefix str)]`
* `PrefixedSnake str` = `CustomJSON '[FieldLabelModifier (StripPrefix str, CamelToSnake)]`
* `Snake` = `CustomJSON '[FieldLabelModifier '[StripPrefix str, CamelToSnake]]`
* `Vanilla` = `CustomJSON '[]`

How it works
----

The wrapper type has a phantom type parameter `t`, a type-level builder of an [Option](http://hackage.haskell.org/package/aeson-1.4.6.0/docs/Data-Aeson.html#t:Options).
Type-level primitives are reduced to one `Option` by the `AesonOptions` class.

```haskell
newtype CustomJSON t a = CustomJSON { unCustomJSON :: a }

class AesonOptions xs where
  aesonOptions :: Options

instance AesonOptions xs => AesonOptions (OmitNothingFields ': xs) where
  aesonOptions = (aesonOptions @xs) { omitNothingFields = True }

...
```

You can use any (static) function for name modification by adding an instance of `StringModifier`.

```haskell
data ToLower
instance StringModifier ToLower where
  getStringModifier "" = ""
  getStringModifier (c : xs) = toLower c : xs
```

Previous studies
----

* [Type-driven safe derivation of ToJSON and FromJSON, using DerivingVia in GHC 8.6 and some type-level hacks](https://gist.github.com/konn/27c00f784dd883ec2b90eab8bc84a81d)
* [Strip prefices from JSON representation](https://gist.github.com/fumieval/5c89205d418d5f9cafac801afbe94969)
