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

# BIO: push and pull

In previous sections, we have introduced the `Z.IO.Buffered` module. And it provides APIs for buffered reading and writing. When combined with [Builder and Parser]() facility, it is easy to handle some simple streaming tasks, for example, read/write packets from TCP wire. But sometimes, things could get complicated. Let's say you want to use the [zlib]() library to decompress a bytes stream from some file. The interface provided by zlib is like this:

```c
int inflateInit (z_streamp strm, int level);
int inflate (z_streamp strm, int flush);
int inflateEnd (z_streamp strm);
```

It's OK to draw a chunk from `BufferedInput`, feed it to `z_streamp`, check the status and do some computation if a decompressed chunk is produced. But how to read a line from decompressed streams? We can't reuse `readLine` from `Z.IO.Buffered` since decompressed chunks are not drawn directly from `BufferedInput`.

Ideally, we should have a composable `BufferedInput` type, which can accept some transformations and yield another `BufferedInput`. But `BufferedInput` is all about managing reading from buffer so that raw byte chunks can be drawn from the device. In Z-IO the `BIO` type is introduced to solve the composable streaming problem:

```haskell
data BIO inp out = BIO
    { push :: inp -> IO (Maybe out)
    , pull :: IO (Maybe out)
    }
```

Conceptually a `BIO` is a box doing data transformation:

```
           +-------------+ 
           |       +-----+-----> Maybe out | Just x : push directly produced an output chunk x
      push |      /      | push            | Nothing: push is not enough to produce an output
 inp ----->+-----+       | 
           | BIO inp out | pull (called after input reached EOF)
           |             +-----> Maybe out | Just x : there's a chunk x left inside BIO's state
           +-------------+                 | Nothing: the BIO reached its EOF after input ended
```

Let's take zlib's `z_streamp` as an example:

+ A `z_streamp` struct could be `push`ed with an input chunk using `inflate`, possibly producing an output chunk. 
+ If input reached EOF, use `inflateEnd` to `pull` the trailing compressed bytes buffered inside `z_streamp` struct.


Another `BIO` example is a rechunk node, which would divide input chunks into chunks with fixed granularity(or multipliers of the fixed granularity). This is often useful in data encryption/decryption. Following code is an implementation of such a node:

```haskell
import Data.IORef
import qualified Z.Data.Vector as V

newReChunk :: Int   -- ^ chunk granularity
           -> IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newReChunk #-}
newReChunk n = do
    -- create a storage for trailing bytes
    trailingRef <- newIORef V.empty
    return (BIO (push_ trailingRef) (pull_ trailingRef))
  where
    -- implement push operation, take input chunk, return chunk in multiplier of granularity
    push_ trailingRef bs = do
        trailing <- readIORef trailingRef
        -- concat trailing bytes from last chunk first
        let chunk =  trailing `V.append` bs
            l = V.length chunk
        if l >= n
        then do     -- if we have enough bytes, then make a cut
            let l' = l - (l `rem` n)
                (chunk', rest) = V.splitAt l' chunk
            writeIORef trailingRef rest
            return (Just chunk')
        else do     -- otherwise we continue waiting for new chunks
            writeIORef trailingRef chunk
            return Nothing

    -- implement pull operation, which is called after input ended
    -- here we choose to return trailing bytes directly
    -- depending on usage, you may throw it away or add some padding 
    pull_ trailingRef = do
        trailing <- readIORef trailingRef
        if V.null trailing
        then return Nothing
        else do
            writeIORef trailingRef V.empty
            return (Just trailing)
```

Look at `newReChunk`'s implementation, which uses `IORef` a.k.a. mutable reference in IO. It's clear that this `BIO` carries its state inside IO monad, in a way similar to `IORef` or `z_streamp` above. So it can't be used like immutable data structures:

```haskell
...
rechunk <- newReChunk
-- used in one place will mutate rechunk's state(trailing bytes in this case)
... rechunk ...
-- it's unsafe to be used in another place 
... rechunk ...
```

The `Z.IO.BIO` module provides various `BIO` node types, from UTF-8 decoder to counter node. Most of them are stateful, and you should create a new node each time. Some nodes are not stateful though:

```haskell
-- A `hexEncoder` is a pure bytes transform node, thus can be used without initialization.
hexEncoder :: Bool  -- ^ uppercase?
           -> BIO Bytes Bytes
hexEncoder upper = pureBIO (hexEncode upper)

-- | BIO node from a pure function.
pureBIO :: (a -> b) -> BIO a b
pureBIO f = BIO (\ x -> let !r = f x in return (Just r)) (return Nothing)
```

# Source and Sink types

Now let's consider the following devices:

+ A data source which doesn't take any input but can be read until EOF.
+ A data sink which only performs writing without producing any meaningful result.

We can have the definitions for data `Source` and `Sink` by using `Void` from `Data.Void`:

```haskell
-- Void from is a data type which doesn't have a constructor
-- Source type doesn't need input
type Source a = BIO Void a
-- Sink type doesn't produce output
type Sink a = BIO a Void
```

Because `Void` type doesn't have constructors, thus `push` `Source` is impossible, one can only define `pull` field when defining a `Source`:

```
           +--------------+ 
 Void --X->|              | 
           | BIO Void out | pull 
           |              +-----> Maybe out | Just x : there's a chunk x left inside `Source`
           +--------------+                 | Nothing: the `Source` reached its EOF
```

For example, a `BIO` node sourcing elements from `[a]` can be implemented like this:

```haskell
{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | Source a list from memory.
sourceFromList :: [a] -> IO (Source a)
sourceFromList xs0 = do
    xsRef <- newIORef xs0
    -- there's no need to set the push field, since no one could possibly call it
    return BIO{ pull = popper xsRef }
  where
    popper xsRef = do
        xs <- readIORef xsRef
        case xs of
            (x:xs') -> do
                writeIORef xsRef xs'
                return (Just x)
            _ -> return Nothing
```

For `type Sink a = BIO a Void`, both `push` and `pull`'s field type is `a -> Maybe Void`, which means both `push` and `pull` can only return `Nothing`. We deliberately use `pull` for flushing output device in Z:

```
           +--------------+ 
           |       +------+-----> Nothing :: Maybe Void
      push |      /       | push (push input chunk into `Sink`)
 inp ----->+-----+        | 
           | BIO inp Void | pull (flush the `Sink`)
           |              +-----> Nothing :: Maybe Void
           +--------------+              
```

# Composing BIO

Now we have the abstract stream transformation type `BIO`, let's start to consider how to compose transformations together instead of `pull` from one and `push` to another manually. The composition of two `BIO` node should:

+ Take the first node's input type, yield second node's output.
+ If an input chunk is `push`ed, produced an output chunk if both node could produce output.
+ After input reached EOF, `pull` should consume buffered trailing bytes from both node.

`>|>` from `Z.IO.BIO` is the implementation of such a composition:

```haskell
                                   Nothing
                                   +--------------------------------+
           +--------------+       /             +--------------+     \
           |       +------+----->+------------->+--------------+------+----> Maybe outB
      push |      /       | push   Just x  push | BIO inB outB |       push
 inA ----->+-----+        |      +--------------+------+       |
           | BIO inA outA | pull | Just x  push | loop |       |       pull
           |              +----->+------------->+------+-------+------+----> Maybe outB
           +--------------+       \             +--------------+     /
                                   +--------------------------------+
                                   Nothing

-- | Connect two 'BIO' nodes, feed the output from left to right.
(>|>) :: BIO a b -> BIO b c -> BIO a c
{-# INLINE (>|>) #-}
BIO pushA pullA >|> BIO pushB pullB = BIO push_ pull_
  where
    push_ inp = do
        -- push to node A first
        x <- pushA inp
        -- if node A produces output, push to node B, otherwise hold
        case x of Just x' -> pushB x'
                  _       -> return Nothing
    pull_ = do
        -- pull node A first
        x <- pullA
        case x of
            -- if node A produces output, push to node B
            Just x' -> do
                y <- pushB x'
                -- draw input from A in a loop until there's an output from B
                case y of Nothing -> pull_  
                          _       -> return y
            -- node A reached EOF, pull node B
            _       -> pullB

```

This composition's type has some interesting results:

+ If you compose a `Source a` to `BIO a b`, you will get a `Source b`.
+ If you compose a `BIO a b` to `Sink a`, you will get a `Sink b`.

So let's say you want to count the line number of a file, you could use `BIO`:

```haskell
import Z.IO
import Z.Data.PrimRef (readPrimIORef)

main :: IO ()
main = do
    _:path:_ <- getArgs
    withResource (initSourceFromFile path) $ \ fileSource -> do
        (counterRef, counterNode) <- newCounterNode
        splitNode <- newLineSplitter
        _ <- runSource_ $ fileSource >|> splitNode >|> counterNode
        printStd =<< readPrimIORef counterRef
```

`runSource_ :: Source a -> IO ()` is a function continuously draw input from a `Source` node until it reaches EOF, it's defined as:

```
-- | Drain a source without collecting result.
runSource_ :: Source x -> IO ()
runSource_ BIO{..} = loop pull
  where
    loop f = do
        r <- f
        case r of Just _ -> loop f
                  _      -> return ()
```

As long as `pull`ing from `Source` returns chunks, `runSource_` will not stop. In the example above, we use this function to drive the whole `BIO` chain, draw chunks from the file and feed into the line splitter and then the counter. The counter state is a primitive reference that can be read using functions from `Z.Data.PrimRef`.

If you have a complete `BIO` from `Source` to `Sink`, you will get a composition node with type `BIO Void Void`, which doesn't have any input or output. You can directly `pull` it to run the whole chain because when you `pull` the composition node, `>|>` will continuously draw chunks from the left node if the right node outputs `Nothing`. And it is the only possible output if the right node is a `Sink`. Thus we have the following function:

```haskell
runBIO :: BIO Void Void -> IO ()
runBIO BIO{..} = pull >> return ()
```

# BIO Cheatsheet

`Z.IO.BIO` provides many functions to construct `Source`, `Sink` and `BIO`s. Here's a cheatsheet:

+ `Source` and `Sink`

    ```haskell
    -- source from list
    sourceFromList                 :: [a] -> IO (Source a)
    -- source from file
    initSourceFromFile             :: CBytes -> Resource (Source Bytes)
    -- source from IO
    sourceFromIO                   :: IO (Maybe a) -> Source a
    -- source from `BufferedInput`
    sourceFromBuffered             :: BufferedInput -> Source Bytes
    sourceTextFromBuffered         :: BufferedInput -> Source Text
    sourceJSONFromBuffered         :: JSON a => BufferedInput -> Source a
    sourceParserFromBuffered       :: Parser a -> BufferedInput -> Source a
    sourceParseChunksFromBuffered  :: Print e
                                   => ParseChunks IO Bytes e a -> BufferedInput -> Source a
    -- sink to list
    sinkToList                     :: IO (IORef [a], Sink a)
    -- sink to file
    initSinkToFile                 :: CBytes -> Resource (Sink Bytes)
    -- sink to perform IO
    sinkToIO                       :: (a -> IO ()) -> Sink a
    -- sink to `BufferedOutput`
    sinkToBuffered                 :: BufferedOutput -> Sink Bytes
    sinkBuilderToBuffered          :: BufferedOutput -> Sink (Builder a)
    ```
+ Built-in `BIO`s

    ```haskell
    -- | Parse bytes to produce result 
    newParserNode :: Parser a -> IO (BIO Bytes a)
    -- | Rechunk chunks to size in multipler of a fixed granularity
    newReChunk :: Int -> IO (BIO V.Bytes V.Bytes)
    -- | UTF-8 decoder
    newUTF8Decoder :: IO (BIO V.Bytes T.Text)
    -- | Split chunk by magic byte(keep the byte)
    newMagicSplitter :: Word8 -> IO (BIO V.Bytes V.Bytes)
    -- | Split chunk by linefeed(drop linefeeds)
    newLineSplitter :: IO (BIO V.Bytes V.Bytes)
    -- | Base64 encoder
    newBase64Encoder :: IO (BIO V.Bytes V.Bytes)
    -- | Base64 decoder
    newBase64Decoder :: IO (BIO V.Bytes V.Bytes)
    -- | Stateless hex encoder
    hexEncoder :: Bool -> BIO V.Bytes V.Bytes
    -- | Hex decoder
    newHexDecoder :: IO (BIO V.Bytes V.Bytes)
    -- | Count input elements number
    newCounterNode :: IO (Counter, BIO a a)
    -- | Label input elements with a sequence number
    newSeqNumNode :: IO (Counter, BIO a (Int, a))
    -- | Grouping input elements into fixed size arrays
    newGroupingNode :: Int -> IO (BIO a (A.SmallArray a))
    ```

+ Composition

    ```haskell
    -- | Connect two 'BIO' nodes, feed left one's output to right one's input.
    (>|>) :: BIO a b -> BIO b c -> BIO a c
    -- | Map a function to BIO's output elements.
    (>~>) :: BIO a b -> (b -> c) -> BIO a c
    -- | Connect BIO to an effectful function.
    (>!>) :: BIO a b -> (b -> IO c) -> BIO a c
    -- | Connect two 'BIO' source, after first reach EOF, draw element from second.
    appendSource :: Source a -> Source a  -> IO (Source a)
    -- | Connect list of 'BIO' sources, after one reach EOF, draw element from next.
    concatSource :: [Source a] -> IO (Source a)
    -- | Zip two 'BIO' source into one, reach EOF when either one reached EOF.
    zipSource :: Source a -> Source b -> IO (Source (a,b))
    -- | Zip two 'BIO' nodes into one, reach EOF when either one reached EOF.
    zipBIO :: BIO a b -> BIO a c -> IO (BIO a (b, c))
    -- | Write to both left and right sink.
    joinSink :: Sink out -> Sink out -> Sink out
    -- | Write to a list of sinks.
    fuseSink :: [Sink out] -> Sink out
    ```

+ Execution

    ```haskell
    -- | Run a 'BIO' chain from source to sink.
    runBIO :: BIO Void Void -> IO ()
    -- | Drain a 'BIO' source into a List in memory.
    runSource :: Source x -> IO [x]
    -- | Drain a source without collecting result.
    runSource_ :: Source x -> IO ()
    -- | Supply a single block of input, then run BIO node until EOF.
    runBlock :: BIO inp out -> inp -> IO [out]
    -- | Supply a single block of input, then run BIO node until EOF with collecting result.
    runBlock_ :: BIO inp out -> inp -> IO ()
    -- | Wrap 'runBlocks' into a pure interface.
    unsafeRunBlock :: IO (BIO inp out) -> inp -> [out]
    -- | Supply blocks of input, then run BIO node until EOF.
    runBlocks :: BIO inp out -> [inp] -> IO [out]
    -- | Supply blocks of input, then run BIO node until EOF with collecting result.
    runBlocks_ :: BIO inp out -> [inp] -> IO ()
    -- | Wrap 'runBlocks' into a pure interface.
    unsafeRunBlocks :: IO (BIO inp out) -> [inp] -> [out]
    ```
