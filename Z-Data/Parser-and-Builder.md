---
layout: default
parent: Z-Data
title: Parser and Builder
nav_order: 3
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

# Parser Monad

The `Parser` from `Z.Data.Parser` is designed for high performance resumable binary parsing and simple textual parsing, such as network protocols, JSON, etc. Write a parser by using basic parsers from `Z.Data.Parser` such as `takeWhile`, `int`, etc.

```haskell
import qualified Z.Data.Parser as P
import Z.Data.ASCII 

data Date = Date { year :: Int, month :: Int, day :: Int } deriving Show

dateParser :: P.Parser Date
dateParser = do
    y <- P.int
    P.word8 HYPHEN 
    m <- P.int
    P.word8 HYPHEN 
    d <- P.int
    return $ Date y m d
```

`Parser` in Z works directly on `Bytes`:

```haskell
> P.parse' dateParser "2020-12-12"
Date 2020 12 12
> P.parse' dateParser "2020-JAN-12"
Left ["Z.Data.Parser.Numeric.int","Z.Data.Parser.Base.takeWhile1: no satisfied byte at [74,65,78,45,49,50]"]
> P.parse dateParser "2020-12-12, 08:00"
([44,32,48,56,58,48,48], Right (Date {year = 2020, month = 12, day = 12}))
> P.parseChunk dateParser "2020-"
Partial _
> let (P.Partial f) = P.parseChunk dateParser "2020-"
> let (P.Partial f') = f "05-05"    -- incrementally provide input
> f' ""                             -- push empty chunk to signal EOF
Success Date {year = 2020, month = 5, day = 5}
```

Binary protocol can use `decodePrim/decodePrimLE/decodePrimBE` with `TypeApplications` extension, let's say you want to implement a [MessagePack str format](https://github.com/msgpack/msgpack/blob/master/spec.md#str-format-family) parser:

```haskell
import           Data.Bits
import           Data.Word
import qualified Z.Data.Parser as P
import qualified Z.Data.Text   as T

msgStr :: P.Parser T.Text
msgStr = do
    tag <- P.anyWord8
    case tag of
        t | t .&. 0xE0 == 0xA0 -> str (t .&. 0x1F)
        0xD9 -> str =<< P.anyWord8
        0xDA -> str =<< P.decodePrimBE @Word16
        0xDB -> str =<< P.decodePrimBE @Word32
        _    -> P.fail' "unknown tag"
  where
    str !l = do
        bs <- P.take (fromIntegral l)
        case T.validateMaybe bs of
            Just t -> return (Str t)
            _  -> P.fail' "illegal UTF8 Bytes"
```

Comparing to `parsec` or `megaparsec`, `Parser` in Z provides limited error reporting, and do not support using as a monad transformer. But provides an instance of `PrimMonad`, which allows some limited effects, such as mutable variables and array operations. 

## Auto Backtracked Alternative

Similar to `attoparsec`, `Parser` in Z always backtrack when used with `<|>` (`Alternative` instance), that means the failed branch will not consume any input without doing anything special:

```haskell
import Control.Applicative
...
p = fooParser <|> barParser <|> quxParser
```

In above code, if any parser failed, the next parser is retried from the beginning of the input. Backtracking is not always needed though, it recommended to use `peek` 
or `peekMaybe` if the syntax or protocol can be parsed as LL(1) grammer since it's faster than backtracking.

# Builder Monad

The `Builder` from `Z.Data.Builder` is the reverse process of parsing, i.e. writing Haskell data types to `Bytes`, aka *Writer* monad. The usage is very similiar to `Parser`:

```haskell
import qualified Z.Data.Builder as B
import Z.Data.ASCII 

data Date = Date { year :: Int, month :: Int, day :: Int } deriving Show

dataBuilder :: Date -> B.Builder ()
dataBuilder (Date y m d) = do
    int' y
    B.word8 HYPHEN 
    int' m
    B.word8 HYPHEN 
    int' d
  where
    int' x | x > 10    = B.int x
           | otherwise = B.word8 DIGIT_0 >> B.int x
```

Underhood a `Builder` records a buffer writing function, thus can be composed quickly. Use `build/buildText` to run a `Builder`, which produces `Bytes` and `Text` respectively:

```haskell
> B.build (dataBuilder $ Date 2020 11 1)
[50,48,50,48,45,49,49,45,48,49]
> B.buildText (dataBuilder $ Date 2020 11 1)
"2020-11-01"
```

Binary `Builder` can be constructed with `encodePrim/encodePrimLE/encodePrimBE`, let's still take [MessagePack str format](https://github.com/msgpack/msgpack/blob/master/spec.md#str-format-family) as an example:

```haskell
import           Data.Bits
import           Data.Word
import qualified Z.Data.Builder as B
import qualified Z.Data.Text    as T
import qualified Z.Data.Vector  as V

msgStr :: T.Text -> B.Builder ()
msgStr t = do
    let bs = T.getUTF8Bytes t
    case V.length bs of
        len | len <= 31      ->  B.word8 (0xA0 .|. fromIntegral len)
            | len < 0x100    ->  B.encodePrim (0xD9 :: Word8, fromIntegral len :: Word8)
            | len < 0x10000  ->  B.encodePrim (0xDA :: Word8, BE (fromIntegral len :: Word16))
            | otherwise      ->  B.encodePrim (0xDB :: Word8, BE (fromIntegral len :: Word32))
    B.bytes bs
```

Note that we directly use `Unalign a, Unalign b => Unalign (a, b)` instance to write serveral primitive types in a row, The `Unalign` class provide basic reading and writing facilities to read primitive types from and to raw bytes(with unaligned offset).

## Text formatting with `Builder`

Different from other standard libraries which usually provide `printf` or similar, in Z directly using `Builder` to format text is recommended:

```haskell
-- Similar to print("The result are %d, %d", x, y)
-- If you can ensure all Builders will write UTF-8 encoded bytes,
-- you can use unsafeBuildText to save a validation

B.unsafeBuildText $ do
    "The result are " >> B.double x >> ", " >> B.double y

-- Or use do syntax

B.unsafeBuildText $ do
    "The result are " 
    B.double x 
    ", " 
    B.double y
...

```

The strength of monadic `Builder` is that you can reuse all control structure from `Control.Monad`, such as conditions, loops, etc.  `Builder ()` has an `IsString` instance which can wrap writing literals in UTF-8 encoding, with some modifications:

+ `\NUL` will be written as `\xC0\x80`.
+ `\xD800` ~ `\xDFFF` will be encoded in three bytes as normal UTF-8 codepoints.

It's safe to put an string literal inside a `unsafeBuildText` as long as you don't write `\0` or `\55296` ~ `\57343`. 
