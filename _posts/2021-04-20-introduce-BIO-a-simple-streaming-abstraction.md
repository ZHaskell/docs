---
layout: post
title:  "Introduce BIO: A Simple Streaming Abstraction"
date:   2021-04-20 14:43:14 CST
author: Dong
categories: design
---

Streaming IO is an old idea: the data is read in chunks, each chunk gets processed and written to output so that the whole memory a program used is kept under a relatively low level. e.g.

```base
cat foo.txt | gzip | base64 | tee foo.gz
```

Above UNIX commands read a file `foo.txt` in chunks, perform gzip and base64 transformation, and get piped to both `foo.gz` and stdout. We'd like to get similar syntax when using Haskell to work with chunked data, and that's the starting point of streaming abstraction.

<!--more-->

## A Stream ADT

### Partial closure 

In [Z-Data's parser section](https://z.haskell.world/Z-Data/Parser-and-Builder.html), we described a resumable parser, which can consume input in chunks:

```haskell
> P.parse' dateParser "2020-12-12"
Date 2020 12 12
> P.parseChunk dateParser "2020-"
Partial _
> let (P.Partial f) = P.parseChunk dateParser "2020-"
> let (P.Partial f') = f "05-05"    -- incrementally provide input
> f' ""                             -- push empty chunk to signal EOF
Success Date {year = 2020, month = 5, day = 5}
```

The core type to achieve resumable parsing is `Result`:

```haskell
data Result e r 
    = Success r !Bytes	 
    | Failure e !Bytes	 
    | Partial (V.Bytes -> Result e r)
```

The `Partial` constructor contains a closure capturing the last chunk's parsing state, which could be applied to the next chunk to produce a new `Result`. Now let's consider if we could apply this construction to IO(or an arbitrary monad), following definition is from the [streaming](https://hackage.haskell.org/package/streaming) package:

```haskell
data Stream f m r = Step !(f (Stream f m r))
                  | Effect (m (Stream f m r))
                  | Return r

data Of a b = !a :> b
```

### Stream Monad

In streaming, `Stream (Of a) IO ()` are used to represent `IO` streams, with some monad primitives you can construct an `IO` stream like this:

```haskell
-- Stream monad will provide some primitives to create monadic value, e.g.
-- yield :: Monad m => a -> Stream (Of a) m ()
-- yield a = Step (a :> Return ())
-- instance (MonadIO m, Functor f) => MonadIO (Stream f m) where
--   liftIO = Effect . fmap Return . liftIO

foo :: Stream (Of a) IO ()
foo = do
    yield 1
    yield 2
    lift readLn >>= yield
``` 

With the `Stream`'s `Monad` instance, the value of foo now becomes a chain of Stream ADTs:

```haskell
Step (1 :> Step (2 :>  Effect (\ x -> Step x :> Return ()) <$> readLn))
```

Now if we provide a function to iterate through this ADT, the stream could be processed. Such a function is often called an interpreter, a term from [the free monad design pattern](https://softwareengineering.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern). For example streaming provides its own `foldrM` interpreter to fold over a `Stream` structure:

```haskell
foldrM :: Monad m => (a -> m r -> m r) -> Stream (Of a) m r -> m r
foldrM step = loop where
  loop stream = case stream of
    Return r       -> return r
    Effect m       -> m >>= loop        -- This is where IO effects happened!
    Step (a :> as) -> step a (loop as)
```

### The Magic Pipes

There're some packages on hackage pushing the free monad technique to its limit, e.g. the [pipes](http://hackage.haskell.org/package/pipes) provide a rather incomprehensible core ADT type:

```haskell
data Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r )
    | Respond b  (b' -> Proxy a' a b' b m r )
    | M          (m    (Proxy a' a b' b m r))
    | Pure    r
```

With this beast at hand, pipes could provide more interesting primitives like `await`, or `>->`. e.g `do x <- await; y <- await; return (x, y)` becomes:

```haskell
Request () (\ x -> Request () (\ x -> Pure (x, y)))
```

One technique pipes used is to use type `Void` to eliminate some constructors under certain types while still keep composability:

```haskell
-- | type with no constructors
type X = Void

-- | 'Effect's neither 'Pipes.await' nor 'Pipes.yield'
type Effect = Proxy X () () X
-- | 'Producer's can only 'Pipes.yield'
type Producer b = Proxy X () () b
-- | 'Pipe's can both 'Pipes.await' and 'Pipes.yield'
type Pipe a b = Proxy () a () b
-- | 'Consumer's can only 'Pipes.await'
type Consumer a = Proxy () a () X
```

## A Retrospective

### Free monad is powerful, but hard to use

The free monad approach could give you as many primitives as you want, and you could choose different interpreter to run, but it's hard to use in several ways:

+ It's hard to comprehend, you have to read the monad instance very carefully, to understand how those primitives work.
+ It has the same problem with monad transformers, i.e. now every base monad operations need to be lifted.
+ It's hard to be optimized by the compiler, because now every operation becomes an ADT constructor, and often leads to higher allocations.

A free monad construction for streaming may also need to provide a different set of combinators, such as `mapM` or `foldM`, which is incompatible with `Control.Monad`.

### How other languages do streaming

It's interesting to find out that most of the OO languages solve this problem in a much simpler way, for example in javascript.

```javascript
// from node.js example
const fs = require('fs');
const zlib = require('zlib');
const r = fs.createReadStream('file.txt');

const z = zlib.createGzip();
const w = fs.createWriteStream('file.txt.gz');
r.pipe(z).pipe(w);

// or you can manually connect streams like this:
r.on('data', (chunk) => { z.write(chunk); });
z.on('data', (chunk) => { w.write(chunk); });
```

In OO's viewpoint, a stream node is an object, with a method to receive chunks, and write to downstream inside callbacks, and that's it. This pattern has some drawbacks:

+ Stream node somehow lost its control, e.g. you can't stop the stream processing in a middle node without touching the source. This is the *Inversion of Control* problem of all callback-based APIs.
+ Stream node now became a mutable stateful object, which is unnatural in Haskell.

## Introduce the BIO

In [Z-IO](https://hackage.haskell.org/package/Z-IO) v0.8, we introduce a new `BIO` type to simplified streaming processing with three design goals:

+ Simple composable types.
+ No transformer, no lift.
+ Easier to be used for writing both processors and applications.

The result is a type focusing on *callback transformation*:

```haskell
-- A bio node receives a callback, returns a new callback to be called from upstream.
type BIO inp out = (Maybe out -> IO ()) -> Maybe inp -> IO ()

-- A Source doesn't consume any meaningful input 
type Source a = BIO Void a
-- A Sink doesn't produce any meaningful output
type Sink a = BIO a Void

-- | A pattern for more meaningful matching.
pattern EOF :: Maybe a
pattern EOF = Nothing
```

For example to implemented a [zlib](https://zlib.net/) node with BIO:

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

When implemented a `Source`, you just ignore the `EOF` param, and call the callback once a new chunk is ready.

```haskell
-- | Turn a `IO` action into 'Source'
sourceFromIO :: HasCallStack => IO (Maybe a) -> Source a
sourceFromIO io = \ k _ ->
    let loop = io >>= \ x ->
            case x of
                Just _ -> k x >> loop   -- you should loop inside a Source
                _      -> k EOF
    in loop
```

You should assume the `EOF` param is only given once, so a loop is often needed. Similar to `Source`, a `Sink` doesn't need to write any output until the final `EOF`:

```haskell
sinkToIO :: HasCallStack => (a -> IO ()) -> Sink a
sinkToIO f = \ k ma ->
    case ma of
        Just a -> f a
        _ -> k EOF
```

### Composing BIO and running

Composing BIO is simple: you can use `(.)` the function composition operator to connect BIOs, since it's just a callback transformation:

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

Above code is similar to command line `cat origin | base | gzip > target`, and `runBIO_` is defined simply as:

```haskell
-- | Discards a value, used as the callback to `Sink`.
discard :: a -> IO ()
discard _ = return ()

runBIO_ :: HasCallStack => BIO inp out -> IO ()
runBIO_ bio = bio discard EOF
```

### Conclusion

There're many streaming libraries on hackage, and most of them are designed around the free monad pattern. In `Z-IO` we introduced a new simpler design around callback transformation, which is much easier to use for writing both stream processors and applications. Of course, nothing is silver bullets. The `BIO` type in `Z-IO` also has limitations, for example, the source can not be paused by a downstream processor without using some IO state, and the whole state management now relies on IO, rather than user-supplied state monads.
