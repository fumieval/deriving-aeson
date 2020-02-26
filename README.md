deriving-aeson
====

[![Hackage](https://img.shields.io/hackage/v/deriving-aeson.svg)](https://hackage.haskell.org/package/deriving-aeson)
![Haskell CI](https://github.com/fumieval/deriving-aeson/workflows/Haskell%20CI/badge.svg)

This package provides a newtype wrapper where you can customise
[aeson](https://hackage,haskell.org/package/aeson)'s generic methods using a
type-level interface, which synergises well with DerivingVia.

```haskell
{-# LANGUAGE DerivingVia, DataKinds, DeriveGeneric #-}
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Deriving.Aeson

data User = User
  { userId :: Int
  , userName :: String
  , userAPIToken :: Maybe String
  } deriving Generic
  deriving (FromJSON, ToJSON)
  via CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "user", CamelToSnake)] User

testData :: [User]
testData = [User 42 "Alice" Nothing, User 43 "Bob" (Just "xyz")]

main = BL.putStrLn $ encode $ testData
-- [{"name":"Alice","id":42},{"api_token":"xyz","name":"Bob","id":43}]
```

Previous studies
----

* https://gist.github.com/konn/27c00f784dd883ec2b90eab8bc84a81d
* https://gist.github.com/fumieval/5c89205d418d5f9cafac801afbe94969
