{-# LANGUAGE DerivingVia, DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Aeson
import Deriving.Aeson
import Deriving.Aeson.Stock
import qualified Data.ByteString.Lazy.Char8 as BL


data User = User
  { userId :: Int
  , userName :: String
  , userAPIToken :: Maybe String
  , userType :: String
  } deriving Generic
  deriving (FromJSON, ToJSON)
  via CustomJSON '[ OmitNothingFields
                  , FieldLabelModifier '[StripPrefix "user", CamelToSnake, Rename "type" "user_type"]
                  ] User

data Foo = Foo { fooFoo :: Int, fooBar :: Int }
  deriving Generic
  deriving (FromJSON, ToJSON)
  via Prefixed "foo" Foo

testData :: [User]
testData = [User 42 "Alice" Nothing "human", User 43 "Bob" (Just "xyz") "bot"]

main = do
  BL.putStrLn $ encode testData
  BL.putStrLn $ encode $ Foo 0 1
