{-# LANGUAGE DerivingVia, DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Deriving.Aeson
import Deriving.Aeson.Stock
import System.Exit (die)
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

data Something = Something { somethingKun :: Int, somethingElseKun :: Int }
  deriving Generic
  deriving (FromJSON, ToJSON) via Suffixed "Kun" Something

testData :: [User]
testData = [User 42 "Alice" Nothing "human", User 43 "Bob" (Just "xyz") "bot"]

data MultipleCtorRenames
  = RenamedCtorOptA
  | RenamedCtorOptB (Maybe ())
  | RenamedCtorOptC Char
  deriving (Eq, Generic, Show)
  deriving (ToJSON)
    via CustomJSON
      [ ConstructorTagModifier (Rename "RenamedCtorOptA" "nullary")
      , ConstructorTagModifier (Rename "RenamedCtorOptB" "twisted-bool")
      , ConstructorTagModifier (Rename "RenamedCtorOptC" "wrapped-char")
      ] MultipleCtorRenames

data MultipleFieldRenames = MultipleFieldRenames
  { fooField1 :: Int
  , fooField2 :: Bool
  , fooField3 :: String
  }
  deriving (Eq, Generic, Show)
  deriving (ToJSON)
    via CustomJSON
      [ FieldLabelModifier (Rename "fooField1" "field-1")
      , FieldLabelModifier (Rename "fooField2" "field-2")
      , FieldLabelModifier (Rename "fooField3" "field-3")
      ] MultipleFieldRenames

main = do
  BL.putStrLn $ encode testData
  BL.putStrLn $ encode $ Foo 0 1
  BL.putStrLn $ encode $ Something 0 1

  assertEq
    (toJSON RenamedCtorOptA)
    (object [("tag", "nullary")])
    "Support multiple constructor modifiers"

  assertEq
    (toJSON $ RenamedCtorOptB Nothing)
    (object [("tag", String "twisted-bool"), ("contents", Null)])
    "Support multiple constructor modifiers"

  assertEq
    (toJSON $ RenamedCtorOptC '?')
    (object [("tag", String "wrapped-char"), ("contents", String "?")])
    "Support multiple constructor modifiers"

  assertEq
    (toJSON $ MultipleFieldRenames 42 True "meaning of life")
    (object [("field-1", Number 42)
            ,("field-2", Bool True)
            ,("field-3", String "meaning of life")
            ])
    "Support multiple field modifiers"

assertEq :: (Show a, Eq a) => a -> a -> String -> IO ()
assertEq x y expectation | x == y = pure ()
                         | otherwise = die msg
  where
    msg = concat [expectation, " -- not fulfilled:\n\t", show x, "\n\t /= \n\t", show y]
