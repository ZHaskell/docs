---
layout: default
parent: Z-Data
title: JSON
nav_order: 4
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

Using `Z.Data.JSON` module to get human readable serialization/deserialization. The easiest way to use the library is to define target data type, deriving
`Generic` and `JSON` instances, which provides:

* `fromValue` to convert `Value` to Haskell values.
* `toValue` to convert Haskell values to `Value`.
* `encodeJSON` to directly write Haskell value into JSON bytes.

```haskell
class JSON a where
  ...
  toValue :: a -> Value
  fromValue :: Value -> Converter a
  encodeJSON :: a -> B.Builder () -- `Z.Data.Builder` as `B`
  ...
```

For example,

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}

import GHC.Generics (Generic)
import qualified Z.Data.Builder as Builder
import qualified Z.Data.JSON as JSON
import qualified Z.Data.Text as T

data Person = Person {name :: T.Text, age :: Int}
    deriving (Show, Generic)
    deriving anyclass (JSON.JSON)
```

We can now encode & decode JSON like this:

```haskell
> JSON.toValue (Person{ name="Alice", age=16 })
Object [("name",String "Alice"),("age",Number 16.0)]
> JSON.encode (Person{ name="Alice", age=16 })
[123,34,110,97,109,101,34,58,34,65,108,105,99,101,34,44,34,97,103,101,34,58,49,54,125]
> JSON.encodeText (Person{ name="Alice", age=16 })
"{\"age\":16,\"name\":\"Alice\"}"
> JSON.decodeText' "{\"age\":16,\"name\":\"Alice\"}" :: Either JSON.DecodeError Person
Right (Person {age = 16, name = "Alice"})
```

The `Generic` based instances convert Haskell data with following rules:

* Constructors without payloads are encoded as JSON String, `data T = A | B` are encoded as `"A"` or `"B"`.
* Single constructor are ingored if there're payloads, `data T = T ...`,  `T` is ingored:
  * Records are encoded as JSON object. `data T = T{k1 :: .., k2 :: ..}` are encoded as `{"k1":...,"k2":...}`.
  * Plain product are encoded as JSON array. `data T = T t1 t2` are encoded as "[x1,x2]".
  * Single field plain product are encoded as it is, i.e. `data T = T t` are encoded as "t" just like its payload.
* Multiple constructors are convert to single key JSON object if there're payloads:
  * Records are encoded as JSON object like above. `data T = A | B {k1 :: .., k2 :: ..}` are encoded as
    `{"B":{"k1":...,"k2":...}}` in `B .. ..` case, or `"A"` in `A` case.
  * Products inside a sum type are similar to above, wrappered by an outer single-key object layer marking which constructor.

These rules apply to user defined ADTs, but some built-in instances have different behaviours, namely:

* `Maybe a` are encoded as JSON `null` in `Nothing` case, or directly encoded to its payload in `Just` case.
* `[a]` are encoded to JSON array, `[Char]` are encoded into JSON string.
* `NonEmpty`, `Vector`, `PrimVector`, `HashSet`, `FlatSet`, `FlatIntSet` are also encoded to JSON array.
* `Bytes` are encoded into JSON text using base64 encoding.
* `HashMap`, `FlatMap`, `FlatIntMap` are encoded to JSON object.

## Custom Settings

There're some modifying options if you providing a custom `Settings`, which
allow you to modify field name or constructor name, but please *DO NOT*
produce control characters during your modification, since we assume field
labels and constructor name won't contain them, thus we can save an extra
escaping pass. To use custom `Settings` just write:

```haskell
data T = T {fooT :: Int, barT :: [Int]} deriving Generic
instance JSON.JSON T where
    -- You can omit following definitions if you don't need to change settings
    toValue = JSON.gToValue JSON.defaultSettings{ JSON.fieldFmt = JSON.snakeCase } . from
    encodeJSON = JSON.gEncodeJSON JSON.defaultSettings{ JSON.fieldFmt = JSON.snakeCase } . from
```

```haskell
> JSON.toValue (T 0 [1,2,3])
Object [("foo_t",Number 0.0),("bar_t",Array [Number 1.0,Number 2.0,Number 3.0])]
```

## Manually Writing Instances

You can write `JSON` instances by hand if the `Generic` based one doesn't suit you.
Here is an example similar to aeson's.

```haskell
import qualified Z.Data.Text          as T
import qualified Z.Data.Vector        as V
import qualified Z.Data.Builder       as B
import qualified Z.Data.JSON          as JSON
import           Z.Data.JSON          ((.:), (.=), (.!), JSON(..))

data Person = Person { name :: T.Text , age  :: Int } deriving Show

instance JSON Person where
    fromValue = JSON.withFlatMapR "Person" $ \ v -> Person
                    <$> v .: "name"
                    <*> v .: "age"

    toValue (Person n a) = JSON.object ["name" .= n, "age" .= a]

    encodeJSON (Person n a) = JSON.object' $ ("name" .! n <> "age" .! a)
```

```haskell
> toValue (Person "Joe" 12)
Object [("name",String "Joe"),("age",Number 12.0)]
> JSON.convert' `Person . JSON.Object $ V.pack [("name",JSON.String "Joe"),("age",JSON.Number 12.0)]
Right (Person {name = "Joe", age = 12})
> JSON.encodeText (Person "Joe" 12)
"{"name":"Joe","age":12}"
```

The `Value` type is different from aeson's one in that we use `Vector (Text, Value)` to represent JSON objects, thus
we can choose different strategies on key duplication, the lookup map type, etc. so instead of a single `withObject`,
we provide `withHashMap`, `withHashMapR`, `withFlatMap` and `withFlatMapR` which use different lookup map type, and different key order piority. Most of time `FlatMap` is faster than `HashMap` since we only use the lookup map once, the cost of constructing a `HashMap` is higher. If you want to directly working on key-values, `withKeyValues` provide key-values vector access.

There're some useful tools to help write encoding code in `Z.Data.JSON.Builder` module, such as JSON string escaping tool, etc. If you don't particularly care for fast encoding, you can also use `toValue` together with value builder, the overhead is usually very small.
