{-# LANGUAGE DerivingVia, DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

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
