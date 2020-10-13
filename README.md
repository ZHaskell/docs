<p align=center>
  Welcome to project Z, an initiative to build a Haskell engineering eco-system.
</p>

## Packages

* [Z-Data](https://github.com/haskell-Z/z-data)

[![Hackage](https://img.shields.io/hackage/v/Z-Data.svg?style=flat)](https://hackage.haskell.org/package/Z-Data) [![Linux Build Status](https://github.com/haskell-Z/z-data/workflows/ubuntu-ci/badge.svg)](https://github.com/haskell-Z/z-data/actions) [![MacOS Build Status](https://github.com/haskell-Z/z-data/workflows/osx-ci/badge.svg)](https://github.com/haskell-Z/z-data/actions) [![Windows Build Status](https://github.com/haskell-Z/z-data/workflows/win-ci/badge.svg)](https://github.com/haskell-Z/z-data/actions)

    * Array, vector(array slice)
    * Text based UTF-8, basic unicode manipulating
    * FFI utilties
    * Parsing and building monad
    * JSON encoding and decoding

* [Z-IO](https://github.com/haskell-Z/z-io)

[![Hackage](https://img.shields.io/hackage/v/Z-IO.svg?style=flat)](https://hackage.haskell.org/package/Z-IO) [![Linux Build Status](https://github.com/haskell-Z/z-io/workflows/ubuntu-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions) [![MacOS Build Status](https://github.com/haskell-Z/z-io/workflows/oxs-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions) [![Windows Build Status](https://github.com/haskell-Z/z-io/workflows/win-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions)

* IO resource management, resource pool
* File system operations
* Network: DNS, TCP, UDP and IPC
* Buffered input and output
* Process management
* Environment settings
* High performance logger
* High performance low resolution timer

* [Z-Botan]((https://github.com/haskell-Z/z-botan)

TLS for haskell based on [botan](https://github.com/randombit/botan), W.I.P

## Requirements

* A working haskell compiler system, GHC(>=8.6), cabal-install(>=2.4), hsc2hs.

* Tests need [hspec-discover](https://hackage.haskell.org/package/hspec-discover).

## Example usage

Check [Z-Example]((https://github.com/haskell-Z/z-example) for examples.

## Dev guide

```bash
# get code
git clone --recursive git@github.com:haskell-Z/Z.git 
cd z-io
# build Z-IO for example
cabal build Z-IO
# test
cabal run Z-IO-Test
# install 
cabal install
# generate document
cabal haddock
```
