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
quickReadFile :: HasCallStack => CBytes -> IO Bytes
quickReadTextFile :: HasCallStack => CBytes -> IO Text
quickWriteFile :: HasCallStack => CBytes -> Bytes -> IO ()
quickWriteTextFile :: HasCallStack => CBytes -> Text -> IO ()
```

`CBytes` is Z's file path type, `Bytes`, `Text` are binary and textual content type respectively, which all be document in Z-Data section. For now, all you need to know is Z-IO assumes UTF-8 encoding everywhere: both filepath and text content are assumed using UTF-8 encoding. 


```haskell
> FS.quickWriteTextFile "./test_file" "hello world!"
> FS.quickReadFile "./test_file" 
[104,101,108,108,111,32,119,111,114,108,100,33]
> FS.quickReadTextFile "./test_file" 
"hello world!"
```

# Resource Handling

Now let's see a more complicated function:

```haskell
initFile :: CBytes
         -> FileFlag        -- ^ Opening flags, e.g. 'O_CREAT' @.|.@ 'O_RDWR'
         -> FileMode        -- ^ Sets the file mode (permission and sticky bits),
                            -- but only if the file was created, see 'DEFAULT_MODE'.
         -> Resource File
```

`FileFlag` and `FileMode` are bit constants controlling the file opening behavior, e.g. if we have read or write access, or if a new file will be created if there's none. You can find more constants on hacakge's document. What's interesting here is that `initFile` function return a `Resource File` type instead of `IO File`. `Resource` is defined in `Z.IO.Resource` module, with a function to use it:

```haskell
withResource :: HasCallStack
             => Resource a      -- ^ resource management record
             -> (a -> IO b)     -- ^ function working on a resource
             -> IO b
```

We simplified those two functions' type a little bit, and here is the idea: `withResource` will take care about resource opening and cleanup automatically, after you finish using it, or when exception happens. You only need to pass a function working on that resource. Now let's read the file created above again:

```haskell
import           Z.IO       -- this module re-export Z.IO.Resource and other common stuff
import qualified Z.IO.FileSystem as FS

withResource (FS.initFile "./test_file" FS.O_RDWR FS.DEFAULT_MODE) $ \ file -> do
    bi <- newBufferedInput file
    printLineStd =<< readLine bi
```

`initFile` function doesn't open the file, it just record how to open and close the file. Everytime you want to do something about the file, use `withResource` to open(and close) it, and that's all about resource handling in Z.

# Buffered I/O

`newBufferedInput` and `readLine` functions in the code above are from `Z.IO.Buffered` module(also re-exported from `Z.IO`). In Z-IO, many IO devices(including `File` above) are instances to `Input/Output` class:

```haskell
class Input i where
    readInput :: HasCallStack => i -> Ptr Word8 -> Int -> IO Int

class Output o where
    writeOutput :: HasCallStack => o -> Ptr Word8 -> Int -> IO ()
```

`readInput` and `writeOutput` works on pointers, which is not very convenient for directly usage. Open a `BufferedInput` or `BufferedOutput` to get auto managed buffered I/O:

```haskell
newBufferedInput :: Input i => i -> IO BufferedInput
newBufferedOutput :: Output i => i -> IO BufferedOutput
```

There's a set of functions working on `BufferedInput/BufferedOutput` in `Z.IO.Buffered`, for example to implement a word counter for files:

```haskell
import           Z.IO       
import qualified Z.IO.FileSystem as FS

main = do
    getArgs
withResource (FS.initFile "./test_file" FS.O_RDWR FS.DEFAULT_MODE) $ \ file -> do
    bi <- newBufferedInput file
    printLineStd =<< readLine bi
```
