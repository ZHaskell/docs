---
layout: default
parent: Z-Data
title: Vector and Text
nav_order: 2
---

# Vector: array slices

In Haskell, we use immutable arrays a lot. And we have many array slices types:

```haskell
-- The payloads are array offset and length
data Vector a = Vector (SmallArray a) Int Int
data PrimVector a = PrimVector (PrimArray a) Int Int 
...
```

These types can support efficiently slicing operations(`take`, `drop`, `break`, etc.), To abstract these types, The `Vec` class is introduced:

```haskell
class (Arr (IArray v) a) => Vec v a where
    -- | Vector's immutable array type
    type IArray v :: Type -> Type
    -- | Get underline array and slice range(offset and length).
    toArr :: v a -> (IArray v a, Int, Int)
    -- | Create a vector by slicing an array(with offset and length).
    fromArr :: IArray v a -> Int -> Int -> v a
```

`Vector` and `PrimVector` are obivious instances, but plain array types are also `Vec`'s instances with `O(n)` `fromArr`, for example:

```haskell
instance Prim a => Vec PrimArray a where
    type IArray PrimArray = PrimArray
    toArr arr = (arr, 0, sizeofArr arr)
    fromArr = fromArray

-- | Construct a slice from an array by copying(if neccessary).
fromArray :: Arr arr a => arr a -> Int -> Int -> arr a
fromArray arr offset len | offset == 0 && sizeofArr arr == len = arr
                         | otherwise = cloneArr arr offset len
```

These instances give `Vec` great flexiblity: if your combinators are implemented with `Vec`, it will works on various slicing types, and plain array types, for example, the `map` combinator from `Z.Data.Vector`:

```haskell
map :: forall u v a b. (Vec u a, Vec v b) => (a -> b) -> u a -> v b
map f (Vec arr s l) = create l (go 0)
  where
    go :: Int -> MArr (IArray v) s b -> ST s ()
    go !i !marr | i >= l = return ()
                | otherwise = do
                    x <- indexArrM arr (i+s); writeArr marr i (f x);
                    go (i+1) marr
```

Note the input and output `Vec` type is not required to be the same, which means applications like the following are possible:

```haskell
data User = User { ..., age :: Int, ...}

-- | Take all user's age and pack them into a `PrimArray`.
takeAllAges :: Vector User -> PrimArray Int
takeAllAges = map age
```

The above functions will work efficiently as expected, `User`'s age will be directly written into a new `PrimArray` with no extra copies.

All functions in `Z.Data.Vector` are implemented using `Vec` constraint, sometimes this will lead to type interference failures, so it's recommended to enable `TypeApplications` extension and add necessary type annotations:

```haskell
{-# LANUAGE TypeApplications #-}

import qualified Z.Data.Vector as V
...
    -- if you don't write annotations, GHC may get confused 
    -- which type of vectors you want to pack.
    let v = V.pack @PrimVector @Word [1..1024]
...
```

# Bytes: Word8 vector

One of the most commonly used vector types is `type Bytes = PrimVector Word8`, which is used to represent binary data. To make writing `Bytes` literals more convenient, `Bytes` is an instance to `IsString`:

```haskell
> import qualified Z.Data.Vector as V
> "hello, world" :: V.Bytes
"hello, world"
```

Note that `Bytes` has a different `Show` instances from other `PrimVector` types using `INCOHERENT` instance feature. In `Z-Data` we use this feature to handle `Bytes`'s JSON instance as well(using base64 encoding). You can use functions from `Z.Data.Vector.Hex` and `Z.Data.Vector.Base64` to manually encode binary `Bytes` into ASCII strings:

```haskell
> import Z.Data.Vector.Hex
> hexEncode True "hello world"
"68656C6C6F20776F726C64"
> import Z.Data.Vector.Base64
> base64Encode "hello wolrd"
"aGVsbG8gd29scmQ="
```

Besides special instances, many functions in `Z.Data.Vector` will leverage rewrite rules to use more efficient instructions when used with `Bytes`, such as `break`, `takeWhile`, etc. But these optimizations should have no visible difference for users.

# Text: UTF-8 encoded Bytes

The `Text` type from `Z.Data.Text` is a `newtype` wrapper around `Bytes` which provides UTF-8 encoding guarantee, you should contrust a `Text` using `validate` or `validateMaybe` only:

```haskell
> import qualified Z.Data.Text as T
> T.validate "hello world"
"hello world"
> T.validate "hello world, \128"
*** Exception: InvalidUTF8Exception [("validate",SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci12", srcLocFile = "<interactive>", srcLocStartLine = 52, srcLocStartCol = 1, srcLocEndLine = 52, srcLocEndCol = 31})]
```

In Haskell, `String`s are allowed to have illegal UTF-8 code points so that any UNIX file path can be encoded in `String`, but in Z.Haskell we have a special type for file path, so `Text` will convert illegal in case of String literals:

```haskell
> "hello world, \55296" :: T.Text
"hello world, �"
> T.getUTF8Bytes "hello world, \55296"  -- surrogates
"hello world, \239\191\189"
```

Note the `\239\191\189` bytes sequence is the replacement char `\U+FFFD`'s UTF-8 encoding form. By providing limited ways of creating `Text`, combinators in `Z.Data.Text` can safely assume `Text` only contain UTF-8 encoded code points.

`Z.Data.Text` also provide some unicode processing capabilities, such as normalization, case-mapping, etc:

```haskell
> T.validate "re\204\129sume\204\129" 
> "résumé" 
> T.normalize (T.validate "re\204\129sume\204\129")
> "résumé" 
> T.getUTF8Bytes $ T.normalize (T.validate "re\204\129sume\204\129")
"r\195\169sum\195\169"
> T.toUpper "διακριτικός"
"ΔΙΑΚΡΙΤΙΚΌΣ"
```

Regex expressions based on [re2](https://github.com/google/re2) regex engine is also provided:

```haskell
> import qualified Z.Data.Text.Regex as RE
> let emailRegex = RE.regex "([a-z0-9_\\.-]+)@([\\da-z\\.-]+)\\.([a-z\\.]{2,6})"
> RE.match emailRegex "hello@world.com"
("hello@world.com",[Just "hello",Just "world",Just "com"],"")
> RE.match emailRegex  "foobar"
("",[],"foobar")
> RE.replace emailRegex True "hello@world.com, foo@bar.com" "x@y.z"
"x@y.z, x@y.z"
> RE.extract  emailRegex "hello@world.com" "http://\\2.\\3"
"http://world.com"
```

# List fusion

`Vec` instances and `Text` support the [build-foldr](https://wiki.haskell.org/Correctness_of_short_cut_fusion#foldr.2Fbuild) fusion by providing fusion rules enabled `pack/unpack`, following code should iterate the input vector and produce the output vector in a single pass rather than producing an intermediate list:

```haskell
f :: V.Vector a -> V.Vector b
f =  V.pack . filter h . map g . V.unpack 
```

This is different from the following code, which will produce an intermediate vector(may not be slower though):

```haskell
f :: V.Vector a -> V.Vector b
f =  V.filter h . V.map' g
```

When working with sequential data, it's recommended to choose vectors as the final representation of data, since it's more compact and GC friendly.

# Type cheatsheet

[Z-Data](https://hackage.haskell.org/package/Z-Data) simplified a lot of types already, but in case of getting confused, here's a type cheat sheet:

```
    +---------------------------------------------------------+
    | Vec class                                               | + Use Array to save ADTs.
    |                                                         | + Use SmallArray if you don't
    |   +----------------------+  +-----------------------+   |   often mutate.
    |   | Arr class            |  | Slice types           |   | + Use PrimArray to save 
    |   |                      |  | support O(1) slicing  |   |   primitive types like 
    |   |  +---------+         |  | with offset/length    |   |   Int or Word8.
    |   |  | Array a |         |  |                       |   | + Use UnliftedArray to save
    |   |  +---------+         |  |                       |   |   unlifted types like
    |   |                      |  |                       |   |   IORef or Array.
    |   |  +---------------+   |  |                       |   | 
    |   |  |UnliftedArray a|   |  |                       |   | + Use slice types to get O(1)
    |   |  +---------------+   |  |                       |   |   slicing operations.
    |   |                      |  |                       |   | + Use Bytes to represent
    |   |  +--------------+    |  |  +----------+         |   |   binary data.
    |   |  | SmallArray a +->arrVec->+ Vector a |         |   |  
    |   |  +--------------+    |  |  +----------+         |   | + Use Text to represent
    |   |                      |  |                       |   |   UTF-8 encoded bytes.
    |   |  +-------------+     |  |  +--------------+     |   |
    |   |  | PrimArray a +->arrVec->-+ PrimVector a |     |   |
    |   |  +-------------+     |  |  +--------------+---+ |   |       
    |   |                      |  |  | Bytes            | |   |       
    |   |                      |  |  | PrimVector Word8 | |   |
    |   |                      |  |  +-------+----------+ |   |       
    |   +----------------------+  +----------V------------+   |
    +----------------------------------------|----------------+
                                          validate
                                             |
                                             V
                                    +--------+------------+
                                    | Text                |
                                    | UTF-8 encoded Bytes |
                                    +---------------------+
```
