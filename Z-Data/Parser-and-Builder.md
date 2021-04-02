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

The `Parser` from `Z.Data.Parser` is designed for high performance resumable binary parsing and simple textual parsing, such as network protocols. It provides limited error reporting functions comparing to parsec or megaparsec, which works on polymorphric token types. `Parser` in Z works directly on `Bytes`:

```
> import qualified Z.Data.Parser as P
> import Z.Data.ASCII 
> :{
| data Date = Date { year :: Int, month :: Int, day :: Int } deriving Show
| let dateParser = do
|         y <- P.int
|         P.word8 HYPHEN 
|         m <- P.int
|         P.word8 HYPHEN 
|         d <- P.int
|         return $ Date y m d
| :}
> P.parse' dateParser "2020-12-12"



```

