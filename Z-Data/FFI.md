---
layout: default
parent: Z-Data
title: FFI
nav_order: 5
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

# FFI: Foreign Function Interface

The Haskell [foreign function interface](https://wiki.haskell.org/Foreign_Function_Interface) is a specification to call foreign functions(mainly C functions) from Haskell. It looks like this:

+ In `Foo.hs`:

    ```haskell
    foreign import ccall unsafe "foo" c_foo :: CInt -> CInt -> IO CInt
    ```

+ In `foo.c`:

    ```c
    int foo(int x, int y){
        ...
    }
    ```

+ In cabal file:

    ```yaml
    ...
        c-sources: foo.c
    ...
    ```

With proper setup, cabal could orchestrate the compilation and give you a static linked binary. The FFI specification specify the concrete syntax in Haskell side, to ensure a successful FFI call, you have to pay attention to several aspects:

+ The types in Haskell and C are matched.
+ How to allocate memory for C side, and when to free.
+ The difference between unsafe FFI calls, and [safe ones](https://simonmar.github.io/bib/papers/conc-ffi.pdf).

Beside above points, you'll have to use correct calling conventions(which would be ccall for most of the time), write C wrappers if you want to call C++, etc.

# FFI Types

Here's a table of common FFI types that can be passed between C and Haskell, and where can you find them:

| C type, header     |  Haskell type, module  | Haskell type(with `UnliftedFFITypes` enable), module |
|--------------------|------------------------|------------------------------------------------------|
| bool, built-in     | CBool, Foreign.C.types | -                                                    |
| int, built-in      | CInt, Foreign.C.types  | -                                                    |
| uint, built-in     | CUInt, Foreign.C.types | -                                                    |                                                   
| long, built-in     | CLong, Foreign.C.types | -                                                    |
| ulong, built-in    | CULong, Foreign.C.types| -                                                    |
| uchar, built-in    | Word8, Data.Word       | -                                                    |
| char, built-in     | Int8, Data.Word        | -                                                    |
| uint8_t, stdint.h  | Word8, Data.Word       | -                                                    |
| uint16_t, stdint.h | Word16, Data.Word      | -                                                    |
| uint32_t, stdint.h | Word32, Data.Word      | -                                                    |
| uint64_t, stdint.h | Word64, Data.Word      | -                                                    |
| int8_t, stdint.h   | Int8, Data.Int         | -                                                    |
| int16_t, stdint.h  | Int16, Data.Int        | -                                                    |
| int32_t, stdint.h  | Int32, Data.Int        | -                                                    |
| int64_t, stdint.h  | Int64, Data.Int        | -                                                    |
| type \*, built-in  | Ptr type, Foreign.Ptr  | Addr#, GHC.Prim                                      |
| HsInt, HsFFI.h     | Int, Prelude           | Int#, GHC.Prim                                       |
| HsWord, HsFFI.h    | Word, Prelude          | Word#, GHC.Prim                                      |
| HsBool, HsFFI.h    | Bool, Prelude          | -                                                    |
| double, built-in   | Double, Prelude        | Double#, GHC.Prim                                    |
| float, built-in    | Float, Prelude         | Float#, GHC.Prim                                     |
| size_t, stddef.h   | CSize, Foreign.C.types | Word#, GHC.Prim                                      |


Some types' size depend on platform(32-bit, 64-bit), e.g. the `HsInt/Int` 's size is 32 bits on 32-bit machine, or 64 bits on 64-bit ones. GHC also support passing some array types to C but not vice versa:

| C type, header     |  Haskell type, module  | Haskell type(with `UnliftedFFITypes` enable), module |
| type \*, built-in  | -                      | MutableByteArray#, GHC.Prim                          |
| const type \*, built-in | -                 | ByteArray#, GHC.Prim                                 |
| StgMutArrPtrs \*(ghc<8.10), StgArrBytes \*\*, Rts.h | - | ArrayArray#, GHC.Prim                    |

The Haskell FFI specification also support function address, which is useful when used as weak pointer's finailizers.

```haskell
foreign import ccall "&free" free :: FunPtr (Ptr Word8 -> IO ())
```

# Allocate and free

It's common to have a C function needs dynamic allocated arrays, there're two solutions in general:

+ Allocate from C side, pass pointer back to Haskell, then use `ForeignPtr` from `Foreign.ForeignPtr` or `CPtr` from `Z.Foreign.CPtr` to wrap it, and ensure the memory will be freed when no longer needed.
+ Allocate from Haskell side as a GC managed heap object, then pass to C for manipulation.

Usually it's recomended to use the second method, since the memory is still under GHC GC's management, so you don't have to worry about free. 

## Allocate memory and pass to C

There're some helpers in `Z.Foreign` to help you with allocating and passing, it's important to have some knowledge about GHC runtime system to get things right. GHC runtime is garbaged collected, and there're two types of primitive array in GHC, with the objective to minimize overall memory management cost:

+ Small primitive arrays created with `newPrimArray` are directly allocated on GHC heap, which can be moved by GHC garbage collector, we call these arrays *unpinned*. Allocating these array is cheap, we only need to check heap limit and bump heap pointer just like any other haskell heap objects. But we will pay GC cost , which is OK for small arrays.

+ Large primitive array and those created with `newPinnedPrimArray` are allocated on GHC managed memory blocks, which is also traced by garbage collector, but will never moved before freed, thus are called *pinned*. Allocating these arrays are bit more expensive since it's more like how malloc works, but we don't have to pay for GC cost.

Beside the pinned/unpinned difference, we have two types of FFI calls in GHC:

+ Safe FFI call annotated with `safe` keyword. These calls are executed on separated OS thread, which can be running concurrently with GHC garbage collector, thus we want to make sure only pinned arrays are passed. The main use case for safe FFIs are long running functions, for example, doing IO polling. Since these calls are running on separated OS thread, haskell thread on original OS thread will not be affected.

+ Unsafe FFI call annotated with `unsafe` keyword. These calls are executed on the same OS thread which is running the haskell side FFI code, which will in turn stop GHC from doing a garbage collection. We can pass both pinned and unpinned arrays in this case. The use case for unsafe FFIs are short/small functions, which can be treated like a fat primitive operations, such as memcpy, memcmp. Using unsafe FFI with long running functions will effectively block GHC runtime thread from running any other haskell threads, which is dangerous. Even if you use threaded runtime and expect your haskell thread can be stolen by other OS threads, but this will not work since GHC garbage collector will refuse to run if one of the OS thread is blocked by FFI calls.

Base on above analysis, we have following FFI strategy table:

| FFI  \ Array |    pinned     |   unpinned    |
|--------------|---------------|---------------|
|   unsafe     | directly pass | directly pass |
|     safe     | directly pass |  make a copy  |

Helpers in `Z.Foreign` are also divided into two categories: those with unsafe suffix to be used with `unsafe` FFI, and those with safe suffix to be used with `safe` FFI. Following is a example to try accommodate a small C function:

```c
include <HsFFI.h>

void c_add_and_time(HsInt x, HsInt y, HsInt* add_result, HsInt* time_result){
    *add_result = x + y;
    *time_result = x * y;
}
```

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnliftedFFITypes #-}

import Z.Foreign

foreign import ccall unsafe c_add_and_time :: Int -> Int -> MBA# Int ->  MBA# Int -> IO ()

cAddTime :: Int -> Int -> (Int, Int)
cAddTime x y = do
    fst <$> allocPrimUnsafe @Int (\ add_result ->
        fst <$> allocPrimUnsafe @Int (\ time_result -> 
            c_add_and_time x y add_result time_result))
```

Now when you call `cAdd` in haskell:

1. `allocPrimUnsafe` function will allocate a single element `MutablePrimArray Int` to be used as `Int` pointer, here we use two `allocPrimUnsafe` to allocate memory for save add and time results.
2. The `x` and `y` parameters are passed as `Int`, and receive as `HsInt` in C. The `add_result` and `time_result` are passed as `MBA# Int`, which is type alias for `MutableByteArray#`, and received as `HsInt*` in C.
3. `allocPrimUnsafe` will auto peek result from the single element array, and return together with FFI's return value, which is ignored by `fst`.

The memory allocated by `allocPrimUnsafe`, `allocPrimArrayUnsafe` and `allocPrimVectorUnsafe` is not pinned, so you can't get the address first, then pass it as `Ptr a`. The only way to pass them is to use `MutableByteArray#` and `ByteArray#` primitive types. In `Z.Foreign` module `BA# a` and `MBA# a` type alias are defined for writing convenience:

```haskell
-- for const pointers
type BA# a = ByteArray#
-- for writable pointers
type MBA# a = MutableByteArray# RealWorld
```

Since they are type aliases, the type tag is only for document. You should use proper pointer types on C side to receive them just like a `Ptr a`. Another common problem with `BA#` and `MBA#` is that they can only pass the array's first element's address, thus you have to manually pass a seperate offset parameter if you want to work with certain range of the array. This can be illustrated by following code:

```c
include <HsFFI.h>

// here we write a wrapper to receive a slice of bytearray
HsInt hs_memchr(const uint8_t *a, HsInt aoff, uint8_t b, HsInt n) {
    a += aoff;
    uint8_t *p = memchr(a, b, (size_t)n);
    if (p == NULL) return -1;
    else return (p - a);
}
```

```haskell
import Z.Foreign
import Data.Word
import qualified Z.Data.Vector as V

foreign import ccall unsafe hs_memchr :: BA# Word8 -> Int -> Word8 -> Int -> IO Int

memchrBytes :: V.Bytes -> Word8 -> Int
memchrBytes bs x = withPrimVector bs $ \ mba off len -> hs_memchr mba off x len
```

The safe FFI variation `withPrimVectorSafe` is simplier, the offset is directly added to the address of pinned memory, so there's only a pointer and an address parameter. It's highly recommended to use unpinned allocation if possible, because pinned allocation often lead to memory fragmentation due their garbage collection strategy, especially under a lot of small repetitive allocations.

## Null terminated strings

C use a lot of null ternimated strings, i.e. `char*` where no length info is needed because it's assumed that the string always ended with a NULL ternimator. In Haskell we provide a special type for this, that is the `CBytes` type from `Z.Data.CBytes` module. Similar to `withPrimVectorUnsafe` and `WithPrimVectorSafe`, use `WithCBytesUnsafe` and `withCBytes` to pass a `CBytes` to C FFI. 

```haskell
> :m + Z.Data.CBytes Z.Foreign Data.Word
> foreign import ccall unsafe strlen :: BA# Word8 -> IO CSize
> withCBytesUnsafe  "hello, world!" strlen
13
> foreign import ccall safe "strlen" strlen_safe :: Ptr Word8 -> IO CSize
> withCBytes "hello, world!" strlen_safe
13
```

Use `allocCBytesUnsafe`, `allocCBytes` to allocate memory to be passed to C, return `CBytes` back.

```haskell
> foreign import ccall unsafe sprint :: MBA# Word8 -> BA# Word8 -> Int -> IO ()
> allocCBytesUnsafe 32 $ \ dest -> withCBytesUnsafe "result is %d" $ \ fmt -> sprintf dest fmt 3
("result is 3",())
```

To get `CBytes` from null terminated `char*`, use `fromCString` or `peekMBACBytes`. If the memory is allocated from C, it's recommend to use `bracket` to ensure memory get freed.

## Unaligned Class

Sometime the memory passed to C are written with some struct fields, you could use `Storable` machinery from `Foreign.Storable` to peek/poke data from/to the memory, but `Storable` use `Ptr a`, so it requires pinned memory whose address is fixed. In [Z-Data](https://hackage.haskell.org/package/Z-Data) an alternative way to do this is to use `Unaligned` class from `Z.Data.Array.Unaligned` module. Here's a code sample from [Z-IO](https://hackage.haskell.org/package/Z-IO):

```c
// definitions from libuv
typedef struct uv_passwd_s {
    char* username;
    long uid;
    long gid;
    char* shell;
    char* homedir;
} uv_passwd_t;

int uv_os_get_passwd(uv_passwd_t* pwd);
void uv_os_free_passwd(uv_passwd_t* pwd);
```

```haskell
import Z.Foreign
import Z.Data.Array.Unaligned
import Z.IO.Exception
import Z.Data.CBytes

-- | Data type for password file information.
data PassWD = PassWD
    { passwd_username :: CBytes
    , passwd_uid :: UID
    , passwd_gid :: GID
    , passwd_shell :: CBytes
    , passwd_homedir :: CBytes
    }   deriving (Eq, Ord, Show, Read)

foreign import ccall unsafe uv_os_get_passwd :: MBA## PassWD -> IO CInt
foreign import ccall unsafe uv_os_free_passwd :: MBA## PassWD -> IO ()

-- | Gets a subset of the password file entry for the current effective uid (not the real uid).
--
-- The populated data includes the username, euid, gid, shell, and home directory.
-- On non-Windows systems, all data comes from getpwuid_r(3).
-- On Windows, uid and gid are set to -1 and have no meaning, and shell is empty.
getPassWD :: HasCallStack => IO PassWD
getPassWD =  bracket
    (do mpa@(MutableByteArray mba##) <- newByteArray (#size uv_passwd_t)
        throwUVIfMinus_ (uv_os_get_passwd mba##)
        return mpa)
    (\ (MutableByteArray mba##) -> uv_os_free_passwd mba##)
    (\ (MutableByteArray mba##) -> do
        username <- fromCString =<< peekMBA mba## (#offset uv_passwd_t, username)
        uid <- fromIntegral <$> (peekMBA mba## (#offset uv_passwd_t, uid) :: IO CLong)
        gid <- fromIntegral <$> (peekMBA mba## (#offset uv_passwd_t, gid) :: IO CLong)
        shell <- fromCString =<< peekMBA mba## (#offset uv_passwd_t, shell)
        homedir <- fromCString =<< peekMBA mba## (#offset uv_passwd_t, homedir)
        return (PassWD username uid gid shell homedir))
```

Note above Haskell code use [hsc2hs](https://hackage.haskell.org/package/hsc2hs) to get constants(struct size, field offset, etc.) from C code, `##` is `#` escaped in `.hsc` file. `uv_os_get_passwd` asks for a `uv_passwd_t*` struct pointer which must a valid writable memory location, so in Haskell we manually allocate memory with `newByteArray` and pass the `MutableByteArray#` as a pointer. After FFI is complete, we use `peekMBA` from `Unaligned` class to read the `char*` pointer, then use
`fromCString` from `Z.Data.CBytes` to copy the result. After copy completes, `uv_os_free_passwd` is called to free any memory allocated in C code.

## CPtr

For some cases, allocation from C is mandatory, e.g. you can't get size to allocate(hidden from C). We will use `CPtr` as an example to illustrate how do we keep reference to some opaque C struct.

First you have to prepare a pair of allocation and free functions:

```c
struct foo_s{
  ...
};

typedef struct foo_s foo_t;

// the allocation function
foo_t *new_foo(int x);

// the free function
void destroy_foo(foo_t* foo);

// some function need foo_t
void bar(foo_t* foo);
```

Now we import these functions in Haskell:

```haskell
import Z.Foreign
import Z.Foreign.CPtr

data Foo

foreign import ccall unsafe new_foo :: CInt -> IO (Ptr Foo)
foreign import ccall unsafe "&destroy_foo" destroy_foo :: FunPtr (Ptr Foo -> IO ())

newFoo :: Int -> IO (CPtr Foo)
newFoo x = newCPtr' (new_foo (fromIntegral x)) destroy_foo

-- use `withCPtr` if you want to get foo_t pointer.
foreign import ccall unsafe bar :: Ptr Foo -> IO ()
...
    foo <- newFoo ...
    withCPtr foo bar
...

```

We encapsulate the C strcut `foo_t` in a Haskell heap object `CPtr Foo` with following steps:

+ Define a type tag `Foo`.
+ Import allocation and free functions, the free function should be imported as a `FunPtr` with its address.
+ Use `newCPtr'` from `Z.Foreign.CPtr` to attach the free function as finalizer, which will be call once the `CPtr Foo` is collected.
+ `withCPtr` will get the pointer back and ensure it will not get collected during the FFI computation.

# Exception handling

 C libraries usually have some conventions on error handling, e.g. return a minus error code to indicate exception case. It's recommend to define an exception type then provide helpers. Following is an example in [Z-Botan](https://github.com/ZHaskell/z-botan):

* Import Error code in hsc file:

```haskell
pattern BOTAN_FFI_ERROR_UNKNOWN_ERROR             :: CInt
pattern BOTAN_FFI_SUCCESS                         = (#const BOTAN_FFI_SUCCESS)
pattern BOTAN_FFI_INVALID_VERIFIER                = (#const BOTAN_FFI_INVALID_VERIFIER)
pattern BOTAN_FFI_ERROR_INVALID_INPUT             = (#const BOTAN_FFI_ERROR_INVALID_INPUT)
...
```

* Define an extensible exception type.

```haskell
data SomeBotanException = forall e . Exception e => SomeBotanException e

instance Show SomeBotanException where
    show (SomeBotanException e) = show e

instance Exception SomeBotanException

botanExceptionToException :: Exception e => e -> SomeException
botanExceptionToException = toException . SomeBotanException

botanExceptionFromException :: Exception e => SomeException -> Maybe e
botanExceptionFromException x = do
    SomeBotanException a <- fromException x
    cast a

#define BotanE(e) data e = e CInt CallStack deriving Show;  \
           instance Exception e where                     \
               { toException = botanExceptionToException     \
               ; fromException = botanExceptionFromException \
               }

BotanE(InvalidVerifier)
BotanE(InvalidInput)
BotanE(BadMac)
...
```

* And provide helpers for FFI code: 

```haskell
throwBotanIfMinus :: (HasCallStack, Integral a) => IO a -> IO a
throwBotanIfMinus f = do
    r <- f
    when (r < 0) (throwBotanError_ (fromIntegral r) callStack)
    return r

throwBotanIfMinus_ :: (HasCallStack, Integral a) => IO a -> IO ()
throwBotanIfMinus_ f = do
    r <- f
    when (r < 0) (throwBotanError_ (fromIntegral r) callStack)

throwBotanError :: HasCallStack => CInt -> IO ()
throwBotanError r = throwBotanError_ r callStack

throwBotanError_ :: CInt -> CallStack -> IO ()
throwBotanError_ r cs =  case r of
    BOTAN_FFI_ERROR_INVALID_INPUT             -> throw (InvalidInput r cs)
    BOTAN_FFI_ERROR_BAD_MAC                   -> throw (BadMac r cs)
    BOTAN_FFI_ERROR_INSUFFICIENT_BUFFER_SPACE -> throw (InsufficientBufferSpace r cs)
    ...
```

* In FFI code, use helper to throw exception when needed:

```haskell
foreign import ccall unsafe hs_botan_mac_update :: BotanStructT -> BA## Word8 -> Int -> Int-> IO CInt

updateMAC :: HasCallStack => MAC -> V.Bytes -> IO ()
updateMAC (MAC bts _ _) bs =
    withBotanStruct bts $ \ pbts ->
        withPrimVectorUnsafe bs $ \ pbs off len ->
            throwBotanIfMinus_ (hs_botan_mac_update pbts pbs off len)
```
