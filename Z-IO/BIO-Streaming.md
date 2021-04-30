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

In previous sections, we have introduced the `Z.IO.Buffered` module. And it provides APIs for buffered reading and writing. When combined with [Builder and Parser]() facility, it is easy to handle some simple streaming tasks, for example, read/write packets from TCP wire. But sometimes, things could get complicated. Let's say you want to use the [zlib](https://zlib.net) library to decompress a bytes stream from some file. The interface provided by zlib is like this:

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
            ... to callback, and you can call callback multiple times.
            callback (Just ...)
            ...
            callback (Just ...)
            ...
        EOF ->
            .. you should pass EOF to callback to indicate current
            .. node also reaches its EOF
            callback EOF
```

`BIO` type have two params:

+ A `callback :: Maybe out -> IO ()`(often written as `k`) which get called when to write downstream:
    + A `Just` value is an item passed to downstream.
    + A `EOF` notified downstream EOF.
+ A `Maybe` value which comes from upstream:
    + A `Just` value is an item from upstream.
    + A `EOF` notified upstream EOF.

Let's take zlib's `z_streamp` as an example to implement a compressing BIO node:

```haskell
compressBIO :: ZStream -> BIO V.Bytes V.Bytes
compressBIO zs = \ callback mbs ->
    case mbs of
        Just bs -> do
            -- feed input chunk to ZStream
            set_avail_in zs bs (V.length bs)
            let loop = do
                    oavail :: CUInt <- withCPtr zs $ \ ps -> do
                        -- perform deflate and peek output buffer remaining
                        throwZlibIfMinus_ (deflate ps (#const Z_NO_FLUSH))
                        (#peek struct z_stream_s, avail_out) ps
                    when (oavail == 0) $ do
                        -- when output buffer is full,
                        -- freeze chunk and call the callback
                        oarr <- A.unsafeFreezeArr =<< readIORef bufRef
                        callback (Just (V.PrimVector oarr 0 bufSiz))
                        newOutBuffer           
                        loop
            loop
        _ -> ... similar to above, with no input chunk and Z_FINISH flag
```

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

Because `Void` type doesn't have constructors, one should ignore the `Maybe Void` param when defining a `Source`. For example, a `BIO` node sourcing chunks from `BufferedInput` can be implemented like this:

```haskell
sourceFromBuffered :: BufferedInput -> Source V.Bytes
sourceFromBuffered i = \ k _ ->
    let loop = readBuffer i >>= \ x ->
            if V.null x then k EOF else k (Just x) >> loop
    in loop
```

For `type Sink a = BIO a Void`, the callback type is `Maybe Void -> IO ()`, which means you can only pass `EOF` to the callback, the convention here is to only call callback when EOF:

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
