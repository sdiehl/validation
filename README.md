# Validation

Extensible validation for a large class of Haskell data structures that can be
composed monoidally, applied applicatively over arbitary datatypes and collect
multiple failure modes in a single validation functor.

```haskell
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
```

See [Example.hs](./Example.hs).
