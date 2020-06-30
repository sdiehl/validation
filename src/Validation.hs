{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Validation
  ( Validation,
    Validator,
    validate,
    positive,
    negative,
    maxLength,
    minLength,
    maxValue,
    minValue,
    eqLength,
    maxTextLength,
    minTextLength,
    textLength,
    eqTextLength,
    isObject,
    isArray,
    isNull,
    hasKey,
    isTrue,
    isFalse,
    regex,
    url,
    email,
    contains,
    excludes,
    decimal,
    toEither,
    fromEither,
    toMaybe,
  )
where

import Data.Aeson hiding (Result, Success)
import qualified Data.HashMap.Strict as HM
import Data.Scientific
import qualified Data.Text as T
import Protolude
import Text.Regex.TDFA

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Ord, Show, Generic, ToJSON)

data Result e = Ok | Err e
  deriving (Eq, Ord, Show, Generic, ToJSON)

newtype Validator e a = Validator (a -> Result e)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Foldable (Validation e) where
  foldMap f (Success a) = f a
  foldMap _ _ = mempty

instance Semigroup err => Applicative (Validation err) where
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e1 <*> _ = Failure e1
  _ <*> Failure e2 = Failure e2
  Success f <*> Success a = Success (f a)

  pure = Success

instance Semigroup e => Semigroup (Validator e a) where
  Validator v1 <> Validator v2 = Validator $ v1 <> v2

instance Monoid e => Monoid (Validator e a) where
  mempty = Validator mempty

instance Semigroup e => Semigroup (Result e) where
  Ok <> Ok = Ok
  Err e1 <> Err e2 = Err $ e1 <> e2
  Err e1 <> _ = Err e1
  _ <> Err e2 = Err e2

instance Monoid e => Monoid (Result e) where
  mempty = Ok

has :: (a -> Bool) -> e -> Validator e a
has p e = Validator (bool Ok (Err e) . p)

refute :: (a -> Bool) -> e -> Validator e a
refute p = has (not . p)

validate :: Validator e a -> a -> Validation e a
validate (Validator f) a = case f a of
  Err e -> Failure e
  Ok -> Success a

toEither :: Validation e a -> Either e a
toEither v = case v of
  Failure e -> Left e
  Success a -> Right a

toMaybe :: Validation e a -> Maybe a
toMaybe v = case v of
  Failure _ -> Nothing
  Success a -> Just a

fromEither :: Either e a -> Validation e a
fromEither e = case e of
  Left a -> Failure a
  Right b -> Success b

-- Validators

isTrue :: e -> Validator e Bool
isTrue = has (== True)

isFalse :: e -> Validator e Bool
isFalse = has (== False)

positive :: (Num a, Ord a) => e -> Validator e a
positive = has (> 0)

negative :: (Num a, Ord a) => e -> Validator e a
negative = has (< 0)

maxValue :: (Num a, Ord a) => a -> e -> Validator e a
maxValue n = refute (> n)

minValue :: (Num a, Ord a) => a -> e -> Validator e a
minValue n = refute (< n)

-- | Verify JSON object
isNull :: e -> Validator e Value
isNull = has (\case Null -> True; _ -> False)

-- | Verify JSON object
isObject :: e -> Validator e Value
isObject = has (\case Object _ -> True; _ -> False)

-- | Verify JSON object
isArray :: e -> Validator e Value
isArray = has (\case Array _ -> True; _ -> False)

-- | Verify JSON object
hasKey :: Text -> e -> Validator e Value
hasKey key = has (\case Object hm -> HM.member key hm; _ -> False)

maxTextLength :: Int -> e -> Validator e Text
maxTextLength n = has (\x -> T.length x < n)

minTextLength :: Int -> e -> Validator e Text
minTextLength n = has (\x -> T.length x > n)

eqTextLength :: Int -> e -> Validator e Text
eqTextLength n = has (\x -> T.length x == n)

textLength :: Semigroup e => Int -> Int -> e -> Validator e Text
textLength n m = minTextLength n <> maxTextLength m

minLength :: Int -> e -> Validator e [a]
minLength n = has (\x -> length x > n)

maxLength :: Int -> e -> Validator e [a]
maxLength n = has (\x -> length x < n)

eqLength :: Int -> e -> Validator e [a]
eqLength n = has (\x -> length x == n)

-- | Verify a string includes a given substring
contains :: Text -> e -> Validator e Text
contains str = has (T.isInfixOf str)

-- | Verify a string excludes a given substring
excludes :: Text -> e -> Validator e Text
excludes str = refute (T.isInfixOf str)

-- | Verify using a regex
regex :: Text -> e -> Validator e Text
regex re = has (=~ re)

-- | Verify url address
url :: e -> Validator e Text
url = has (=~ urlRegex)
  where
    urlRegex :: Text
    urlRegex = "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)"

-- | Verify email address
email :: e -> Validator e Text
email = has (=~ emailRegex)
  where
    emailRegex :: Text
    emailRegex = "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+"

-- | Check that a scientific value has no more than maxDigits and has exponent
-- less than max
decimal :: (Int, Int) -> e -> Validator e Scientific
decimal (maxDigits, maxExp) = has verifier
  where
    verifier x =
      let (ds, power) = toDecimalDigits x
       in (length ds < maxDigits) && (power < maxExp)
