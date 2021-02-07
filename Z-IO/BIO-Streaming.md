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

# `BIO`: push and pull

In previous sections we introduced the `Z.IO.Buffered` module, which gives you buffered reading and writing. Combined with [Buidler and Parser]() facility, It's easy to handle some simple streaming task such as read/write packets from TCP wire. But sometime things could get complicated. Let's say you want to use the [zlib]() library to decompress a bytes stream from file. The interface provided by zlib is like this:

```c
int inflateInit (z_streamp strm, int level);
int inflate (z_streamp strm, int flush);
int inflateEnd (z_streamp strm);
```

It's OK to draw a chunk from `BufferedInput` and feed it to `z_streamp`, check the status and do computation if a decompressed chunk is produced. But how to read a line from decompressed streams? We can't reuse `readLine` from `Z.IO.Buffered` since decompressed chunks are not directly drawed from `BufferedInput`.

Ideally we should have a composable `BufferedInput` type, which can accept some transformations and yield another ` `BufferedInput`. But `BufferedInput` is all about managing reading buffer so that raw bytes chunks can be drawed from device. In Z-IO the `BIO` type is introduced to solve the composable streaming problem:

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

+ A `z_streamp` struct could be `push`ed with an input chunk using `inflate`, possibly produced an output chunk. 
+ If input reached EOF, use `inflateEnd` to `pull` the trailing compressed bytes buffered inside `z_streamp` struct.


Another `BIO` example is a rechunk node, which would divide input chunks into chunks with fixed granularity(or multipler of the fixed granularity). This is often useful in data encryption/decryption. Following code is an implementation of such a node:

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
    -- implement push operation, which can receive new input chunk
    push_ trailingRef bs = do
        trailing <- readIORef trailingRef
        -- there maybe trailing bytes from last chunk
        let chunk =  trailing `V.append` bs
            l = V.length chunk
        if l >= n
        then do
            -- if we have enough bytes, then make a cut
            let l' = l - (l `rem` n)
                (chunk', rest) = V.splitAt l' chunk
            writeIORef trailingRef rest
            return (Just chunk')
        else do
            -- otherwise we continue waiting for new chunks
            writeIORef trailingRef chunk
            return Nothing

    -- implement pull operation, which is called after input ended
    pull_ trailingRef = do
        trailing <- readIORef trailingRef
        if V.null trailing
        then return Nothing
        else do
            writeIORef trailingRef V.empty
            -- here we choose to directly return trailing bytes
            -- depend on usage, you may throw it away, or add some padding 
            return (Just trailing)
```

Look at `newReChunk`'s implementation, which use `IORef` a.k.a. mutable reference in IO, It's clear that this `BIO` carry its state inside IO monad, in a way similar to `IORef` or `z_streamp` above. So it can't be used like immutable data structures:

```haskell
...
rechunk <- newReChunk

-- used in one place will mutate rechunk's state
... rechunk ...
-- so it's not safe to be used in another place
... rechunk ...
```

The `Z.IO.BIO` module provides various `BIO` node types, from UTF-8 decoder to counter node. Most of them are stateful, you have to create a new node each time. Some nodes are not stateful though:

```haskell
hexEncoder :: Bool  -- ^ uppercase?
           -> BIO Bytes Bytes
```

A `hexEncoder True` is a `BIO` node wrap a pure bytes transform function, thus can be used multiple time without initialization.

# `Source` and `Sink` types

Now we consider following devices:

+ A data source is a `BIO` node which doesn't take input, but can be read until EOF.
+ A data sink is a `BIO` node which only perform writing without producing any meaningful result.

Then we can have the definitions for data `Source` and `Sink`s:

```haskell
-- Void from `Data.Void` is a data type which doesn't have a constructor,
-- data Void
-- `Source` type doesn't need input
type Source a = BIO Void a
-- `Sink` type doesn't produce output
type Sink a = BIO a Void
```

Because `Void` type doesn't have constructors, we can't possibly `push` a `Void` to `Source`, one can omit `push` field when defining a `Source`, for example if we want to source element from `[a]` :

```haskell
{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | Source a list from memory.
sourceFromList :: [a] -> IO (Source a)
sourceFromList xs0 = do
    xsRef <- newIORef xs0
    -- there's no need to set the push field, since 
    -- no one could possibly call it
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



# Compose `BIO`s

Now we have a abstract stream transformation type BIO, we can start to consider how to compose these transformations together instead of `pull` from one and `push` to another manually. The composition of two `BIO` node should:

+ Take the first node's input type, yield second node's output.
+ If an input chunk is `push`ed, produced an output chunk if both node could produce output.
+ After input reached EOF, `pull` should consume buffered trailing bytes from both node.

Here is the implementation of such a composition:

```haskell
                                   Nothing
                                   +--- ----------------------------+
           +--------------+       /             +--------------+     \
           |       +------+----->+------------->+--------------+------+----> Maybe outB
      push |      /       | push   Just x  push | BIO inB outB |       push
 inA ----->+-----+        |      +--------------+------+       |
           | BIO inA outA | pull | Just x  push | loop |       |       pull
           |              +----->+------------->+------+-------+------+----> Maybe outB
           +--------------+       \             +--------------+     /
                                   +--------------------------------+
                                   Nothing

-- | Connect two 'BIO' nodes, feed left one's output to right one's input.
(>|>) :: BIO a b -> BIO b c -> BIO a c
{-# INLINE (>|>) #-}
BIO pushA pullA >|> BIO pushB pullB = BIO push_ pull_
  where
    push_ inp = do
        -- push to node A first
        x <- pushA inp
        -- if node A produces output, push to node B
        -- otherwise hold
        case x of Just x' -> pushB x'
                  _       -> return Nothing
    pull_ = do
        -- pull node A first
        x <- pullA
        case x of
            -- if node A produces output, push to node B
            Just x' -> do
                y <- pushB x'
                -- draw input from A until there's an output from B
                case y of Nothing -> pull_  
                          _       -> return y
            -- node A reached EOF, pull node B
            _       -> pullB

```


