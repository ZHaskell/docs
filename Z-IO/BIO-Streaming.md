---
layout: default
parent: Z-IO
title: BIO Streaming
nav_order: 3
---

In previous sections we introduced the `Z.IO.Buffered` module, which gives you buffered reading and writing. Combined with [Buidler and Parser]() facility, It's easy to handle some simple streaming task such as read/write packets from TCP wire. But sometime things could get complicated. Let's say you want to use the [zlib]() library to decompress a bytes stream from file. The interface provided by zlib is like this:

```C
int inflateInit (z_streamp strm, int level);
int inflate (z_streamp strm, int flush);
int inflateEnd (z_streamp strm);
```

It's OK to draw a chunk from `BufferedInput` and feed it to `z_stream_t`, check the status and do computation if a decompressed chunk is produced. But how to read a line from decompressed streams? We can't reuse `readLine` from `Z.IO.Buffered` since decompressed chunks are not directly drawed from `BufferedInput`.

Ideally we should have a composable `BufferedInput` type, which can accept some transformations and yield another ` `BufferedInput`. But `BufferedInput` is all about managing reading buffer so that raw bytes chunks can be drawed from device. In Z-IO the `BIO` type is introduced to solve the composable streaming problem:

```haskell
data BIO inp out = BIO
    { push :: inp -> IO (Maybe out)
    , pull :: IO (Maybe out)
    }
```

Conceptually a `BIO` is a box doing data transformation:

```
           +-------------+ push
           |       +-----+-----> Maybe out | Just x : push directly produced an output chunk x
      push |      /      |                 | Nothing: push is not enough to produce an output
 inp ----->+-----+       | 
           | BIO inp out | pull (called after input reached EOF)
           |             +-----> Maybe out | Just x : there's a chunk x left inside BIO's state
           +-------------+                 | Nothing: the BIO reached its EOF after input ended
```

W.I.P
