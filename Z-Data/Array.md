---
layout: default
parent: Z-Data
title: Array
nav_order: 1
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

# Array in Haskell

Unlike the ubiquitous linked list type `[a]`. In Haskell arrays doesn't have any built-in syntax support, or any other special compiler support expects some built-in primitive functions, which can be found in [ghc-prim](http://hackage.haskell.org/package/ghc-prim/docs/GHC-Prim.html):

```haskell
newArray# :: Int# -> a -> State# s -> (# State# s, MutableArray# s a #)
readArray# :: MutableArray# s a -> Int# -> State# s -> (# State# s, a #)
writeArray# :: MutableArray# s a -> Int# -> a -> State# s -> State# s
newByteArray# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
indexInt8Array# :: ByteArray# -> Int# -> Int#
indexInt16Array# :: ByteArray# -> Int# -> Int#
...
```

It's hard to directly use those functions because they directly manipulate `State#` token, and they distinguish different array types: boxed `Array#`, `ByteArray#`, etc. The `#` after those types imply they are special primitive types, which will be discussed later.

In [Z-Data](https://hackage.haskell.org/package/Z-Data)，we provide type wrappers and typeclass to unified array operations:

```haskell
class Arr (arr :: * -> * ) a where
    -- | Mutable version of this array type.
    type MArr arr = (mar :: * -> * -> *) | mar -> arr
    -- | Make a new array with given size.
    newArr :: (PrimMonad m, PrimState m ~ s) => Int -> m (marr s a)
    -- | Make a new array and fill it with an initial value.
    newArrWith :: (PrimMonad m, PrimState m ~ s) => Int -> a -> m (marr s a)
    -- | Index mutable array in a primitive monad.
    readArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m a
    -- | Write mutable array in a primitive monad.
    writeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> a -> m ()
    -- | Fill mutable array with a given value.
    setArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> a -> m ()
    -- | Index immutable array, which is a pure operation,
    indexArr :: arr a -> Int -> a
    -- | Index immutable array in a primitive monad, this helps in situations that
    -- you want your indexing result is not a thunk referencing whole array.
    indexArrM :: (Monad m) => arr a -> Int -> m a
    -- | Safely freeze mutable array by make a immutable copy of its slice.
    freezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (arr a)
    -- | Safely thaw immutable array by make a mutable copy of its slice.
    thawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> Int -> Int -> m (marr s a)
    -- | In place freeze a mutable array, the original mutable array can not be used
    -- anymore.
    unsafeFreezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m (arr a)
    -- | In place thaw a immutable array, the original immutable array can not be used
    -- anymore.
    unsafeThawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> m (marr s a)
    -- | Copy a slice of immutable array to mutable array at given offset.
    copyArr ::  (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> arr a -> Int -> Int -> m ()
    -- | Copy a slice of mutable array to mutable array at given offset.
    -- The two mutable arrays shall no be the same one.
    copyMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
    -- | Copy a slice of mutable array to mutable array at given offset.
    -- The two mutable arrays may be the same one.
    moveArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
    -- | Create immutable copy.
    cloneArr :: arr a -> Int -> Int -> arr a
    -- | Create mutable copy.
    cloneMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (marr s a)
    -- | Resize mutable array to given size.
    resizeMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m (marr s a)
    -- | Shrink mutable array to given size. This operation only works on primitive arrays.
    -- For boxed array, this is a no-op, e.g. 'sizeOfMutableArr' will not change.
    shrinkMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m ()
    -- | Is two mutable array are reference equal.
    sameMutableArr :: marr s a -> marr s a -> Bool
    -- | Size of immutable array.
    sizeofArr :: arr a -> Int
    -- | Size of mutable array.
    sizeofMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m Int
    -- | Is two immutable array are referencing the same one.
    sameArr :: arr a -> arr a -> Bool
```

And we have following instances:

```haskell
-- | Boxed array type, for holding haskell ADTs.
instance Arr Array a where
    type MArr Array = MutableArray
    ...
-- | Boxed array type, for holding haskell ADTs, but doesn't carry a card table.
instance Arr SmallArray a where
    type MArr SmallArray = SmallMutableArray
    ...
-- | Unboxed array type, for holding primitive types like Int, Word8, etc.
instance Prim a => Arr PrimArray a where
    type MArr PrimArray = MutablePrimArray
    ...
-- | Boxed array type, for holding boxed unlifted types, see following section.
instance PrimUnlifted a => Arr UnliftedArray a where
    type MArr UnliftedArray = MutableUnliftedArray
    ...
```

If you know how `IO` works in Haskell, `PrimMonad` simply means `ST` or `IO`. But if you get confused by the `PrimMonad` constraint, please get [more details here](https://wiki.haskell.org/IO_inside).

# Boxed, Unboxed

For many haskellers, using arrays may be the first time one wants to know what's the difference between boxed, unboxed types. It's important to spend some time explaining these buzzwords.

In other languages, you often have to distinguish *reference* and *value*. For example, in C pointers are references to other objects. It's a memory location in hardware sense: you can use machine code to follow a reference to the memory it pointing to. While the other non-pointer types value are not memory locations, their 1-0 arrangements stands for a certain value of that type.

In haskell almost every value you see is a pointer from C's perspective, i.e. a memory location point to a heap object, for example a data type like:

```haskell
data Foo = Foo Int Char
foo = Foo 3 'a'
```

Are represented as:

```
    foo(from registers or other boxes)
     |
     V
+----+--------+---+---+    +-------------+------+
| info-table* | * | * +--->+ info-table* | 'a'# |
+-------------+-+-+---+    +-------------+------+
  Foo           |           C# (Char's constructor)
                V
            +---+---------+----+
            | info-table* | 3# |
            +-------------+----+
             I# (Int's constructor)
```

During runtime the value `foo` is a reference, and all the operations, e.g. pattern match, go through dereferencing. Values like this are called *boxed* because it's a reference to a box, i.e. heap objects with [info-table](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects#info-tables). The info-table contains many useful infomation about the box, such as how many words the boxed occupied, which constructor the box stand for, etc.

The `3#` and `'a'#` above are haskell's non-pointer value, we call values like this *unboxed* values. Unboxed values don't have info-tables, so we really can't have them directly on heap: otherwise the GC would get confused when it scans them: without infomation from info-table, it can't decide how many bytes to copy. These values are usually belong to registers or other boxes: we generate machine code to manipulate them directly.


## Boxed array

Now let's consider GHC arrays, they're special heap objects provided by RTS. We have boxed arrays `MutableArray#` and `Array#` that store references to boxes:

```
+-------------+--------------+---------------------------+---+-...-+---+---+------------+
| info-table* | payload size | payload + card-table size | * | ... | * | * | card table |
+-------------+--------------+---------------------------+-+-+-...-+---+---+------------+
 MutableArray#                                             |
 Array#                                                    V
                                                    +------+------+-----+
                                                    | info-table* | ... |
                                                    +-------------+-----+
                                                      Boxes, maybe a thunk
                                                      Most of the operations on boxed array
                                                      are lazy on its element
```

It looks quite complicated, especially the card-table part, which is used to [optimize the GC for arrays](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/gc/remembered-sets). `MutableArray#`s are always kept in a generation's mutable list once it's promoted to that generation, so this optimization is important if you keep a large mutable array on heap for a long time. For small arrays, it's unnecessary to use a card-table, and GHC provides `MutableSmallArray#/SmallArray#` for that purpose.

```
+-------------+--------------+---+-...-+---+---+
| info-table* | payload size | * | ... | * | * | 
+-------------+--------------+---+-...-+---+---+
 MutableSmallArray#
 SmallArray#
```

There're ADT wrappers for these types to make it easier to work with:

```haskell
data MutableArray s a = MutableArray (MutableArray# s a)
data Array a = Array (Array# a)

data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)
data SmallArray a = SmallArray (SmallArray# a)
```

A common pattern in Haskell is to turn `MutableArray` into an `Array` with freeze operations after creation complete, but the card-table's space is still there in case we thaw the array in place again. Generally speaking, under creation-freeze pattern, `MutableSmallArray` and `SmallArray` are more recommended since you won't keep mutable array on heap for too long.

## Unboxed array

`MutableByteArray#`, `ByteArray#` are GHC's unboxed array. They don't contain pointers, and their payload do not need to be traced during GC:

```
+-------------+--------------+-------------+---+-...-+---+---+
| info-table* | payload size | 0xXXXXXXXX# | # | ... | # | # |
+-------------+--------------+-------------+---+-...-+---+---+
 MutableByteArray#
 ByteArray#   
```

`ByteArray#`s can be used to encode different size non-pointer data, such as `Int` and `Word8`, `ghc-prim` provide seperated functions to work with different data types: `indexIntArray#`, `indexWord8Array#`, etc, So there're `Prim` class and `PrimArray` type to make working with different types easier:

```haskell
-- types which can be stored in ByteArray# 
class Prim a where
    indexByteArray# :: ByteArray# -> Int# -> a
    ...

-- | type indexed ByteArray#
data PrimArray a = PrimArray ByteArray#

indexPrimArray :: Prim a => PrimArray a -> Int -> a
...
```

# Lifted, Unlifted

Another difference between types: unlifted and lifted, exists because in haskell we have non-strict evaluation mechanism, e.g. a value `1 + 2` may have a representation like:

```
+-------------+----------+---+    +-------------+----+
| info-table* | reserved | * +--->+ info-table* | 2# |
+------+------+----------+---+    +-------------+----+
       |                           This is I#
       V
 The info-table points to (+1) code.
```

In Haskell `1 + 2` and `3` are both references, they can be used interchangeably: a function expecting an `Int` argument can accept both pointers. This is done by *entering* the heap objects. i.e. execute the entry code following the info-table. The entry code for constructors are simply returns. For thunks the code will do evaluation and the `reserved` word above is reserved exactly for evaluation result, by writing a forward pointer and change the thunk box into an indirection box.

The evaluation may fail(diverged recursion, stackoverflow, etc.), so the pointer could potentially point to an undefined value, this kind of things are called *bottom* in haskell, written as `_|_`. The intuition for this name is that all the other evaluated values have certain meaning, but bottom doesn't, it sits lower in the spectrum of determinism, concreteness, usefulness ... whatever suits your mind. Hence comes the concept of `lifted` type, i.e. types which contain `bottom` values, or more formly, inhabited by `_|_`.

As you expected, most of the boxed type can be inhabited by `_|_`, the thunk may explode and terminate your program, or call `error` or `undefined` in base. And most of the unboxed types are unlifted types. e.g. It's impossible that an `Int#` would stand for an undefined value, because all 1-0 arrangements would represent a `Int#`, or put it another way: there's no way we get a bottom from `Int#`, because it doesn't have an info-table, and we can't enter it.

But some boxed unlifted types do exist, e.g. `MutableArray#/Array#` are such types, their representation on heap have an info-table pointer, but they were never entered. All the primitive operations manipulating them won't enter them, and the only way to create them is via `newArray#`, `cloneArray#`, etc. 

To efficiently store boxed unlifted types, `Unlifted` class and `UnliftedArray` type are introduced similar to `Prim` and `PrimArray`, `UnliftedArray` store unlifted references instead of normal haskell ADTs. Comparing `Array Array`, `UnliftedArray Array` could remove a level of redirection, i.e. remove item's `Array` box and store `Array#` directly.

# More on arrays

There're more details on Haskell arrays, such as pinned vs unpinned `ByteArray`s, etc. Interested readers could find all these details on [GHC wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/home), especially on RTS section.
To use array properly, all you need to do is choose the proper storage type and import `Z.Data.Array`. In next section we will introduce vectors, which is simply slices of arrays.
