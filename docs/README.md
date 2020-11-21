# Overview

Project Z is a set of BSD licensed high-performance Haskell libraries, providing basic data structures and I/O operations based on [libuv OS abstractions and GHC runtime's lightweight thread](https://dl.acm.org/doi/abs/10.1145/3242744.3242759).

## Z Packages

* [Z-Data](https://github.com/haskell-Z/z-data)

  [![Hackage](https://img.shields.io/hackage/v/Z-Data.svg?style=flat)](https://hackage.haskell.org/package/Z-Data) [![Linux Build Status](https://github.com/haskell-Z/z-data/workflows/ubuntu-ci/badge.svg)](https://github.com/haskell-Z/z-data/actions) [![MacOS Build Status](https://github.com/haskell-Z/z-data/workflows/osx-ci/badge.svg)](https://github.com/haskell-Z/z-data/actions) [![Windows Build Status](https://github.com/haskell-Z/z-data/workflows/win-ci/badge.svg)](https://github.com/haskell-Z/z-data/actions)

  * Array, vector(array slice), Bytes(Word8 vectors)
  * Text based UTF-8, basic unicode manipulating
  * FFI utilties
  * Parsing and building monad
  * JSON encoding and decoding

* [Z-IO](https://github.com/haskell-Z/z-io)

  [![Hackage](https://img.shields.io/hackage/v/Z-IO.svg?style=flat)](https://hackage.haskell.org/package/Z-IO) [![Linux Build Status](https://github.com/haskell-Z/z-io/workflows/ubuntu-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions) [![MacOS Build Status](https://github.com/haskell-Z/z-io/workflows/osx-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions) [![Windows Build Status](https://github.com/haskell-Z/z-io/workflows/win-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions)

  * IO resource management, resource pool
  * File system operations
  * Network: DNS, TCP, UDP and IPC
  * Buffered input and output
  * Process management
  * Environment settings
  * High performance logger
  * High performance low resolution timer

* [Z-Botan](https://github.com/haskell-Z/z-botan)

  TLS for haskell based on [botan](https://github.com/randombit/botan), W.I.P

## Requirements

* A working haskell compiler system, GHC(>=8.6), cabal-install(>=2.4), hsc2hs. 

    * Ubuntu users can get them via [ppa](https://launchpad.net/~hvr/+archive/ubuntu/ghc).
    * Mac users can get them via homebew: `brew install ghc cabal-install`.
    * Windows users can get them via chocolatey: `choco install ghc cabal`
    * Other users can get them from [here](https://www.haskell.org/ghc/).


## User Guide

To use Z-IO for example, add following lines to your `project.cabal` file:

```
...
    build-depends:          Z-IO  == 0.1.*
```

Now run `cabal build` within your project directory, cabal should be able to download Z-IO dependency automatically. You
can check [Z-Example](https://github.com/haskell-Z/z-example) for some example code.

## Developer Guide

It's recommended to clone this repo, it contains all components as git submodules:

```bash
# get code
git clone --recursive git@github.com:haskell-Z/Z.git 
cd z-io
git checkout master
git pull
# build Z-IO for example
cabal build Z-IO
# test
cabal test
# generate document
cabal haddock
```

To run tests, you should have [hspec-discover](https://hackage.haskell.org/package/hspec-discover) installed.
Run `cabal` within Z folder will build project directly using source code. Pull requests should be sent
independently to each repo. Welcome join us!
