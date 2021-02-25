---
layout: default
parent: Z-IO
title: Filesystem
nav_order: 1
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

# Hello File

It's easy to use Z-IO package's filesystem module, first please import `Z.IO.Filesystem`:

```haskell
import qualified Z.IO.FileSystem as FS
```

If it's OK to load a file into memory at once, you can use following:

```haskell
readFile :: HasCallStack => CBytes -> IO Bytes
readTextFile :: HasCallStack => CBytes -> IO Text
writeFile :: HasCallStack => CBytes -> Bytes -> IO ()
writeTextFile :: HasCallStack => CBytes -> Text -> IO ()
```

`CBytes` is Z's file path type. `Bytes`, and `Text` are types for binary and textual content, respectively. And these are all documented in Z-Data section. For now, all you need to know is Z-IO assumes UTF-8 encoding everywhere: both filepath and text content can be assumed this way. 


```haskell
> FS.writeTextFile "./test_file" "hello world!"
> FS.readFile "./test_file" 
[104,101,108,108,111,32,119,111,114,108,100,33]
> FS.readTextFile "./test_file" 
"hello world!"
```

# Resource Handling

Now let's see a more complicated function:

```haskell
initFile :: CBytes
         -> FileFlag        -- ^ Opening flags, e.g. 'O_CREAT' @.|.@ 'O_RDWR'
         -> FileMode        -- ^ Sets the file mode (permission and sticky bits),
                            -- but only if the file was created, see 'DEFAULT_FILE_MODE'.
         -> Resource File
```

`FileFlag` and `FileMode` are bit constants controlling the file opening behavior, such as if we have read or write access or if a new file will be created when there's none. You can find more constants on hackage. The interesting thing here is that `initFile` function returns a `Resource File` type instead of `IO File`. `Resource` is defined in `Z.IO.Resource` module, with a function to use it:

```haskell
withResource :: HasCallStack
             => Resource a      -- ^ resource management record
             -> (a -> IO b)     -- ^ function working on a resource
             -> IO b

withResource' :: HasCallStack
              => Resource a      -- ^ resource management record
              -> (a -> IO () -> IO b)   
                    -- ^ second param is the close function for early closing
              -> IO b
```

We simplified those two functions' type a little bit, and here is the idea: `withResource` will take care of resource opening and cleanup automatically, after you finish using it, or when exceptions happen. You only need to pass a function working on that resource. Now let's read the file created above again:

```haskell
import           Z.IO       -- this module re-export Z.IO.Resource and other common stuff
import qualified Z.IO.FileSystem as FS

withResource (FS.initFile "./test_file" FS.O_RDWR FS.DEFAULT_FILE_MODE) $ \ file -> do
    bi <- newBufferedInput file
    printStd =<< readLine bi
```

`initFile` function doesn't open the file, and it just records how to open and close the file. Every time you want to do something with the file, use `withResource` to open(and close) it, and that's all about resource handling in Z.

# Buffered I/O

`newBufferedInput` and `readLine` functions in the code above are from `Z.IO.Buffered` module(also re-exported from `Z.IO`). In Z-IO, many IO devices(including `File` above) are instances of `Input/Output` class:

```haskell
class Input i where
    readInput :: HasCallStack => i -> Ptr Word8 -> Int -> IO Int
class Output o where
    writeOutput :: HasCallStack => o -> Ptr Word8 -> Int -> IO ()
```

`readInput` and `writeOutput` work on pointers, which is not very convenient for direct usage. Open a `BufferedInput` or `BufferedOutput` to get auto-managed buffered I/O:

```haskell
newBufferedInput :: Input i => i -> IO BufferedInput
newBufferedOutput :: Output o => o -> IO BufferedOutput
```

There's a set of functions working on `BufferedInput/BufferedOutput` in `Z.IO.Buffered`, for example, to implement a word counter for files:

```haskell
import           Z.IO       
import qualified Z.IO.FileSystem    as FS
import qualified Z.Data.Vector      as V

main :: IO ()
main = do
    -- get file path from command line
    (_:path:_) <- getArgs
    withResource (FS.initFile path FS.O_RDWR FS.DEFAULT_FILE_MODE) $ \ file -> do
        bi <- newBufferedInput file
        printStd =<< loop bi 0
  where
    loop :: BufferedInput -> Int -> IO Int
    loop input !wc = do
        -- read a single line with linefeed dropped
        line <- readLine input
        case line of
            Just line' ->
                loop input (wc + length (V.words line'))
            _ -> return wc
```

Here's a quick cheatsheet on buffered IO, `BufferedInput` first:

```haskell
-- | Request a chunk from the input device.
readBuffer :: HasCallStack => BufferedInput -> IO Bytes

-- | Push back an unconsumed chunk
unReadBuffer :: HasCallStack => Bytes -> BufferedInput -> IO ()

-- | Read exactly N bytes, throw exception if EOF reached before N bytes.
readExactly :: HasCallStack => Int -> BufferedInput -> IO Bytes

--  /----- readToMagic ----- \ /----- readToMagic -----\ ...
-- +------------------+-------+-----------------+-------+
-- |       ...        | magic |       ...       | magic | ...
-- +------------------+-------+-----------------+-------+
readToMagic :: HasCallStack => Word8 -> BufferedInput -> IO Bytes

--  /--- readLine ---\ discarded /--- readLine ---\ discarded / ...
-- +------------------+---------+------------------+---------+
-- |      ...         | \r\n/\n |       ...        | \r\n/\n | ...
-- +------------------+---------+------------------+---------+
readLine :: HasCallStack => BufferedInput -> IO (Maybe Bytes)

-- | Read all chunks from input.
readAll :: HasCallStack => BufferedInput -> IO [Bytes]
readAll' :: HasCallStack => BufferedInput -> IO Bytes

-- | See Parser & Builder under Z-Data section for the following functions.
-- | Request input using Parser
readParser :: HasCallStack => Parser a -> BufferedInput -> IO a

-- | Request input using ParseChunks, see Parser & Builder under Z-Data section.
readParseChunks :: (Print e, HasCallStack) => ParseChunks IO Bytes e a -> BufferedInput -> IO a
```

`BufferedOutput` is relatively simple:

```haskell
-- | Write a chunk into buffer.
writeBuffer :: HasCallStack => BufferedOutput -> Bytes -> IO ()
-- | Directly write Builder into output device.
writeBuilder :: HasCallStack => BufferedOutput -> Builder a -> IO ()
-- | Flush the buffer into output device.
flushBuffer :: HasCallStack => BufferedOutput -> IO ()
```

# A note on filepath

Other operations from `Z.IO.FileSystem` module, e.g., `seek`, `mkdtemp`, `rmdir`, etc., are basically mirroring the Unix system call, which should be familiar to people who come from C/C++. The type for file path in Z is `CBytes`, which is a `\NUL` terminated byte array managed on GHC heap. 

We assumed that `CBytes`'s content is UTF-8 encoded though it may not always be the case, and there're some platform differences on file path handling, e.g., the separator on windows is different from Unix. To proper handle file path, use `Z.IO.FileSystem.FilePath` (which is re-exported from `Z.IO.FileSystem`), for example, instead of manually connecting file path like:

```haskell 
let p = "foo" <> "/" <> "bar" 
```
You should always use functions from the library

```haskell
import qualified Z.IO.FileSystem as FS

let p = "foo" `FS.join` "bar" 
-- "foo" `FS.join` "../bar" will yield "bar" instead of "foo/../bar"
```
