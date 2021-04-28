---
layout: default
parent: Z-IO
title: BIO Streaming
nav_order: 3
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

# BIO: composable callbacks

In previous sections, we have introduced the `Z.IO.Buffered` module. And it provides APIs for buffered reading and writing. When combined with [Builder and Parser]() facility, it is easy to handle some simple streaming tasks, for example, read/write packets from TCP wire. But sometimes, things could get complicated. Let's say you want to use the [zlib]() library to decompress a bytes stream from some file. The interface provided by zlib is like this:

```c
int inflateInit (z_streamp strm, int level);
int inflate (z_streamp strm, int flush);
int inflateEnd (z_streamp strm);
```

It's OK to draw a chunk from `BufferedInput`, feed it to `z_streamp`, check the status and do some computation if a decompressed chunk is produced. But how to read a line from decompressed streams? We can't reuse `readLine` from `Z.IO.Buffered` since decompressed chunks are not drawn directly from `BufferedInput`.

Ideally, we should have a composable `BufferedInput` type, which can accept some transformations and yield another `BufferedInput`. But `BufferedInput` is all about managing reading from buffer so that raw byte chunks can be drawn from the device. In Z-IO the `BIO` type is introduced to solve the composable streaming problem:

```haskell
type BIO inp out = (Maybe out -> IO ()) -> Maybe inp -> IO ()
```

Conceptually a `BIO` is a box doing transformation on data callbacks:

```haskell
-- A pattern synonym for more meaningful pattern match
pattern EOF :: Maybe a
pattern EOF = Nothing

fooBIO :: BIO foo bar
fooBIO callback maybeFoo = do
    ... use callback to pass output data
    case maybeFoo of
        Just foo ->
            ... you can send result to downstream by pass Just values
            ... to callback, and you can call callback multiple time.
            callback (Just ...)
            ...
            callback (Just ...)
            ...
        EOF ->
            ... Nothing input indicate EOF
            .. you should pass EOF to callback to indicate current
            .. node also reaches its EOF
            callback EOF
```

Let's take zlib's `z_streamp` as an example:

+ A `z_streamp` struct could be `push`ed with an input chunk using `inflate`, possibly producing an output chunk. 
+ If input reached EOF, use `inflateEnd` to `pull` the trailing compressed bytes buffered inside `z_streamp` struct.

The `Z.IO.BIO` module provides various `BIO` node types, from UTF-8 decoder to counter node. Most of them are stateful, so you should create a new node each time.

# Source and Sink types

Now let's consider the following devices:

+ A data source which doesn't take any input but can be read until EOF.
+ A data sink which only performs writing without producing any meaningful result.

We can have the definitions for data `Source` and `Sink` by using `Void` from `Data.Void`:

```haskell
-- Source type doesn't need input
type Source a = BIO Void a
-- Sink type doesn't produce output
type Sink a = BIO a Void
```

Because `Void` type doesn't have constructors, thus `push` values other than `Nothing` to `Source` is impossible, one should ignore the `Maybe Void` param when defining a `Source`. For example, a `BIO` node sourcing chunks from `BufferedInput` can be implemented like this:

```haskell
sourceFromBuffered :: BufferedInput -> Source V.Bytes
sourceFromBuffered i = \ k _ ->
    let loop = readBuffer i >>= \ x ->
            if V.null x then k EOF else k (Just x) >> loop
    in loop
```

For `type Sink a = BIO a Void`, the callback type is `Maybe Void -> IO ()`, which means you can only pass `Nothing` to the callback, the convention here is to only call callback once with `Nothing` on EOF:

```haskell
-- | The `BufferedOutput` device will get flushed only on EOF.
sinkToBuffered :: BufferedOutput -> Sink V.Bytes
sinkToBuffered bo = \ k mbs ->
    case mbs of
        Just bs -> writeBuffer bo bs
        _       -> flushBuffer bo >> k EOF
```

# Composing BIO

The `BIO` type could be composed via `(.)`, i.e. the function composition. The composition's result has some interesting facts:

+ If you compose a `Source a` to `BIO a b`, you will get a `Source b`.
+ If you compose a `BIO a b` to `Sink b`, you will get a `Sink a`.

So let's say you want to count the line number of a file, you could use `BIO`:

```haskell
import Z.IO
import Z.Data.PrimRef 

main :: IO ()
main = do
    _:path:_ <- getArgs
    withResource (initSourceFromFile path) $ \ fileSource -> do
        counterRef <- newCounter 0
        let counter = counterNode counterRef
        splitter <- newLineSplitter
        runBIO_ $ fileSource . splitter . counter
        printStd =<< readPrimIORef counterRef
```

`runBIO_ :: Source a -> IO ()` simply supply a `EOF` to the BIO chain, and fileSource will drive the whole chain running until EOF, it's defined as:

```haskell
discard :: a -> IO ()
{-# INLINABLE discard #-}
discard _ = return ()

runBIO_ :: BIO inp out -> IO ()
{-# INLINABLE runBIO_ #-}
runBIO_ bio = bio discard EOF
```

Another example from the [introduce BIO blog post](https://z.haskell.world/design/2021/04/20/introduce-BIO-a-simple-streaming-abstraction.html):

```haskell
import Z.Data.CBytes    (CBytes)
import Z.IO
import Z.IO.BIO
import Z.IO.BIO.Zlib

base64AndCompressFile :: HasCallStack => CBytes -> CBytes -> IO ()
base64AndCompressFile origin target = do
    base64Enc <- newBase64Encoder
    (_, zlibCompressor) <- newCompress defaultCompressConfig{compressWindowBits = 31}

    withResource (initSourceFromFile origin) $ \ src ->
        withResource (initSinkToFile target) $ \ sink ->
            runBIO_ $ src . base64Enc . zlibCompressor . sink
```

Above code is similar to command line `cat origin | base | gzip > target`.
