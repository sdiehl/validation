# Validation

![Cabal CI](https://github.com/adjoint-io/validation/workflows/Cabal%20CI/badge.svg)
![Stack CI](https://github.com/adjoint-io/validation/workflows/Stack%20CI/badge.svg)

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

## License

```
Copyright (c) 2019-2020 Adjoint Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
OR OTHER DEALINGS IN THE SOFTWARE.
`
