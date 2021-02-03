---
layout: post
title:  "High-performance JSON codec"
date:   2021-02-01 16:52:44 CST
author: Dong
categories: performance 
---

JSON processing is a fundamental building block in modern network applications. It's also a large module in [Z-Data](//hackage.haskell.org/package/Z-Data) package. With careful optimization, we managed to get a 1.5X - 3X encoding and 3X decoding performance boost comparing to [aeson](//hackage.haskell.org/package/aeson), a widely used JSON package on hackage.
 
 <!--more-->

## Benchmark Result

![bench-result](https://github.com/ZHaskell/benchmarks/blob/master/json-benchmark/json-benchmark-result.png?raw=true)

The [above benchmarks](//github.com/ZHaskell/z-benchmarks) running on an MBP13 2020(2 GHz Quad-Core Intel Core i5), Each benchmark runs a certain JSON task with fixed iterations, using [sample data](//github.com/ZHaskell/benchmarks/tree/master/asset/json-data). Some notes on benchmarks code:
    
* Benchmarks labeled with `encode` and `decode` bench the conversion between JSON documents and JSON intermedia representation.
* Benchmarks labeled with `typed encode` and `typed decode` bench the conversion between JSON documents and Haskell ADT.
* All ADTs' instances are deriving using GHC generic mechanism, no manual conversion code is required. 

## Fast escaping handling

Surprisingly, when processing JSON, one can't directly copy strings because they may be [escaped](https://tools.ietf.org/html/rfc8259#page-8), which brings a quite big performance challenge. In [Z-Data](//hackage.haskell.org/package/Z-Data) we carefully arranged the code path to avoid performance hit:

* When encoding text value

    1. Run a prescan loop to find if we need escaping, and how much space we need to write the escaped string if escaping is needed. 
    2. If there's no escaping needed, vectorized `copyByteArray#` is used to directly write text into the output buffer.
    3. Otherwise, go through the escaping loop.

* When decoding JSON string

    1. Run a prescan to find the end of the string, record if unescaping is needed at the same time.
    2. If no unescaping is needed, a vectorized UTF8 validation is used.
    3. Otherwise, go through a UTF8 validation loop extended with JSON unescaping logic. 

These optimizations are possible because [Z-Data](//hackage.haskell.org/package/Z-Data) uses UTF8 encoding `Text` type, which could save considerable time on the non-escaping path.

## IR(intermedia represatation)

Another optimization opportunity comes from the new JSON document IR design. In [Z-Data](//hackage.haskell.org/package/Z-Data) the IR type use vector of key-value pair to represent JSON objects:

```haskell
data Value = Object (Vector (Text, Value))
           | Array  (Vector Value)
           | String T.Text
           | Number Scientific
           | Bool   Bool
           | Null
         deriving (Eq, Ord, Show, Typeable, Generic)
         deriving anyclass Print
```

This representation has many benefits:

* Preserve original key-value order, so that round-trip processing is possible.
* User can choose different de-duplicate strategys when converting IR to ADT.
* It's faster to construct an IR value or convert ADT to IR.

By default [Z-Data](//hackage.haskell.org/package/Z-Data) use [FlatMap](//hackage.haskell.org/package/Z-Data/docs/Z-Data-Vector-FlatMap.html) when converting IR to ADT, which is simply a sorted vector of key-value pair. It can be constructed by sorting the original key-value pairs in O(N\*logN) and looked up using binary-search in O(logN).

## Parser and Builder facility

[Z-Data](//hackage.haskell.org/package/Z-Data) uses [Bytes](https://hackage.haskell.org/package/Z-Data/docs/Z-Data-Vector.html#t:Bytes), a vector type based on `ByteArray#` to represent binary data, it's different from traditional bytestring ones that use `Addr#`(pointer). It's necessary to provide a different set of `Builder`s and `Parser`s to work on that representation. In both cases, simple CPSed monad is chosen to make compiled code fast.

```
-- Z.Data.Builder.Base
newtype Builder a = Builder { 
    runBuilder :: (a -> BuildStep)  -- next write continuation
               -> BuildStep
}

-- Z.Data.Parser.Base
newtype Parser a = Parser {
    runParser :: forall r . (ParseError -> ParseStep r)     -- fail continuation
              -> (a -> ParseStep r)                         -- success continuation
              -> ParseStep r
}
```

These types are almost the simplest CPS monads one can write, and GHC is particularly good at optimizing the composition of these monads. 

## Conclusion

This benchmark compared [Z-Data](//hackage.haskell.org/package/Z-Data) to widely used Haskell package [aeson](//hackage.haskell.org/package/aeson). The result shows that the new `Builder` and `Parser` facility works as expected, and our optimizing techniques can bring a huge performance improvement.


