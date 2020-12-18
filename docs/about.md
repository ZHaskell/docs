---
layout: page
title: About
nav_order: 1
permalink: /about/
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

Project Z is a set of BSD licensed high-performance Haskell libraries, providing basic data structures and I/O operations based on [libuv OS abstractions and GHC runtime's lightweight thread](//dl.acm.org/doi/abs/10.1145/3242744.3242759).

## Requirements

You need a working haskell compiler system: GHC(>=8.6), cabal-install(>=2.4). 

* Ubuntu users can get them via [ppa](//launchpad.net/~hvr/+archive/ubuntu/ghc).
* Mac users can get them via homebew: `brew install ghc cabal-install`.
* Windows users can get them via chocolatey: `choco install ghc cabal`
* Other users can get them from guide [here](//www.haskell.org/ghc/).

## User Guide

To use Z-IO for example, add following lines to your `project.cabal` file:

```
...
    build-depends:          Z-IO  == 0.1.*
```

Now run `cabal build` within your project directory, cabal should be able to download Z-IO dependency automatically. You
can check [Z-Example](//github.com/haskell-Z/z-example) for some example code.

## Developer Guide

It's recommended to clone [this repo](//github.com/haskell-Z/Z), it contains all components as git submodules:

```bash
# get code
git clone --recursive git@github.com:haskell-Z/Z.git 
# build Z-IO for example
cd z-io
git checkout master
git pull
# build
cabal build
# test
cabal test
# generate document
cabal haddock
```

To run tests, you should have [hspec-discover](//hackage.haskell.org/package/hspec-discover) installed.
Pull requests should be sent to each repo. Welcome join us!

