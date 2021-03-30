---
layout: default
parent: Z-Data
title: Vector and Text
nav_order: 2
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

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
> :set -XOverloadedStrings
> "hello, world" :: V.Bytes
"hello, world"
> "你好世界" :: V.Bytes     -- unicode literals will be get choped!
"`}\SYNL"
```

In the above example, unicode literals "你好世界" do not produce UTF-8 encoded byte vector as one might expect, you have to use `Text` to get that behaviour:

```haskell
> import qualified Z.Data.Text as T
> T.getUTF8Bytes "你好世界" 
"\228\189\160\229\165\189\228\184\150\231\149\140"
```

Note that `Bytes`'s `Show` instance is not specialized to show ASCII characters. You can use functions from `Z.Data.Vector.Hex` and `Z.Data.Vector.Base64` to manually encode binary `Bytes` into ASCII strings:

```haskell
> import Z.Data.Vector.Hex
> hexEncode True "hello world"
"68656C6C6F20776F726C64"
> import Z.Data.Vector.Base64
> base64Encode "hello wolrd"
"aGVsbG8gd29scmQ="
```

In `Z-Data` we use incoherent instance to handle `Bytes`'s JSON instance(using base64 encoding):

```haskell
> V.pack [0..127] :: V.Bytes 
"\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\DEL"
> V.pack [0..127] :: V.PrimVector Int
[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127]
> import qualified Z.Data.JSON as JSON
> JSON.encode (V.pack [0..127] :: V.Bytes)
"\"AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG1ub3BxcnN0dXZ3eHl6e3x9fn8=\""
> JSON.encode (V.pack [0..127] :: V.PrimVector Int)
"[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127]"
```

Besides special instances, many functions in `Z.Data.Vector` will leverage rewrite rules to use more efficient instructions when used with `Bytes`, such as `break`, `takeWhile`, etc. But these optimizations should have no visible difference for users.

# Text: UTF-8 encoded Bytes

The `Text` type from `Z.Data.Text` is a `newtype` wrapper around `Bytes` which provides UTF-8 encoding guarantee, you should contrust a `Text` using `validate` or `validateMaybe` or string literals only:

```haskell
> import qualified Z.Data.Text as T
> T.validate "hello world"
"hello world"
> T.validate "hello world, \128"
*** Exception: InvalidUTF8Exception [("validate",SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci12", srcLocFile = "<interactive>", srcLocStartLine = 52, srcLocStartCol = 1, srcLocEndLine = 52, srcLocEndCol = 31})]
> "你好世界" :: T.Text
"你好世界"
```

In Haskell, `String`s are allowed to have illegal UTF-8 code points so that any UNIX file path can be encoded in `String`, but in Z.Haskell we have a special type for file path. `Text` will convert illegal code points in case of string literals:

```haskell
> "hello world, \55296" :: T.Text
"hello world, �"
> T.getUTF8Bytes "hello world, \55296"  -- surrogates
"hello world, \239\191\189"
```

The `\239\191\189` bytes sequence is the replacement char `\U+FFFD`'s UTF-8 encoding form. By providing limited ways of creating `Text`, combinators in `Z.Data.Text` can safely assume `Text` only contain UTF-8 encoded code points.

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
