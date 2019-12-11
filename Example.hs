{-# LANGUAGE ApplicativeDo #-}

module Example where

import Data.Aeson
import Data.Scientific
import Protolude
import Validation

-- Extensible validation for a large class of Haskell data structures that can
-- be combined monoidally and fail in multiple ways.

-----------------------------------------------------
-- Validate Records
-----------------------------------------------------

data InvalidFoo
  = NegativeX
  | PositiveY
  deriving (Show)

data Foo = Foo {x :: Int, y :: Int}
  deriving (Show)

validateFoo :: Foo -> Validation [InvalidFoo] Foo
validateFoo (Foo a b) =
  Foo <$> validate (positive [NegativeX]) a
      <*> validate (negative [PositiveY]) b

checkFoo :: Foo -> Either [InvalidFoo] Foo
checkFoo = toEither . validateFoo

-----------------------------------------------------
-- Validate Decimals
-----------------------------------------------------

data InvalidNumber = TooBig
  deriving (Show)

checkDecimal :: Scientific -> Either InvalidNumber Scientific
checkDecimal = toEither . validate (decimal (5, 10) TooBig)

-----------------------------------------------------
-- Validate URLs
-----------------------------------------------------

data InvalidUrl = InvalidUrl
  deriving (Show)

checkUrl :: Text -> Either InvalidUrl Text
checkUrl = toEither . validate (url InvalidUrl)

-----------------------------------------------------
-- Validate passwords
-----------------------------------------------------

data PasswordError
  = InvalidLength
  | ContainsUppercase
  | ContainsLowercase
  | ContainsNumber
  deriving (Show)

passwordValidate :: Validator [PasswordError] Text
passwordValidate =
     textLength 1 8 [InvalidLength]
  <> regex "[A-Z]" [ContainsUppercase]
  <> regex "[a-z]" [ContainsLowercase]
  <> regex "[0-9]" [ContainsNumber]

checkPassword :: Text -> Either [PasswordError] Text
checkPassword = toEither . validate passwordValidate

-----------------------------------------------------
-- Validate JSON
-----------------------------------------------------

data JsonError = ExpectingObject
  deriving (Show)

checkJSON1 :: Maybe (Validation JsonError Value)
checkJSON1 = validate (isObject ExpectingObject) <$> decode "[1,2,3]"

checkJSON2 :: Maybe (Validation JsonError Value)
checkJSON2 = validate (isObject ExpectingObject) <$> decode "{}"

checkJSON3 :: Maybe (Validation JsonError Value)
checkJSON3 = validate (hasKey "a" ExpectingObject) <$> decode "{\"a\": 1}"
