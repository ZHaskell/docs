[Edit this page](https://github.com/haskell-Z/Z/blob/master/docs/Z.IO.Filesystem.md)

# Hello file

It's easy to use Z-IO package's file APIs, let's import `Z.IO.Filesystem` first:

```
import qualified Z.IO.FileSystem as FS
```

It's recommended to import filesystem module qualified as `FS`, If there's no too much trouble loading a file to memory at once, you can use following:

```
quickReadFile :: HasCallStack => CBytes -> IO Bytes
quickReadTextFile :: HasCallStack => CBytes -> IO Text
quickWriteFile :: HasCallStack => CBytes -> Bytes -> IO ()
quickWriteTextFile :: HasCallStack => CBytes -> Text -> IO ()

```

Here `CBytes` is Z's file path type, `Bytes`, `Text` are binary and textual content type respectively, which all be document in Z-Data section. For now, all you need to know is Z assume UTF-8 encoding everywhere, and both filepath and text content file should use UTF-8 encoding. 


```
> FS.quickWriteTextFile "./test_file" "hello world!"
> FS.quickReadFile "./test_file" 
[104,101,108,108,111,32,119,111,114,108,100,33]
> FS.quickReadTextFile "./test_file" 
"hello world!"
```

# Resource Handling

Now let's see a more complicated function:

```
initFile :: CBytes
         -> FileFlag        -- ^ Opening flags, e.g. 'O_CREAT' @.|.@ 'O_RDWR'
         -> FileMode        -- ^ Sets the file mode (permission and sticky bits),
                            -- but only if the file was created, see 'DEFAULT_MODE'.
         -> Resource File
```

`FileFlag` and `FileMode` are bit constants controlling the file opening behavior, e.g. if we have read or write access? or if a new file will be created if there's none. You can find more constants in hacakge's document. What's interesting here is that this function return a `Resource File` type instead of `IO File`. `Resource` is defined in `Z.IO.Resource`, there's a function to use it:

```
withResource :: HasCallStack
             => Resource a      -- ^ resource management record
             -> (a -> IO b)     -- ^ function working on a resource
             -> IO b
```

We simplified those two functions' type a little bit, but you get the idea: `withResource` will take care about resource opening and cleanup, even exception happens. All you need to do is pass a function working directly on the resource itself. Now let's read the file created above again:

```
import           Z.IO       -- this module re-export Z.IO.Resource and other common stuff
import qualified Z.IO.FileSystem as FS

withResource (FS.initFile "./test_file" FS.O_RDWR FS.DEFAULT_MODE) $ \ file -> do
    bi <- newBufferedInput file
    printLineStd =<< readLine bi
```

Just remember `initFile` function doesn't open the file, it just record how to open and close the file, and everytime you want to do something about the file, use `withResource` to handle it, and that's all about resource handling in Z.

# Buffered I/O

Now let's take a look on `newBufferedInput` function from `Z.IO.Buffered`(which is also re-exported from `Z.IO`):

```
newBufferedInput :: Input i => i -> IO BufferedInput
newBufferedOutput :: Output i => i -> IO BufferedOutput
```

There'is a set of functions working on `BufferedInput`(and `BufferedOutput`) in `Z.IO.Buffered`:

```


```
