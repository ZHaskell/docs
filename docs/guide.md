---
layout: page
title: Guide
nav_order: 1
permalink: /guide
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

## Requirements

You need a working haskell compiler system: GHC(>=8.6), cabal-install(>=2.4). You can download pre-built binaries([GHC](https://www.haskell.org/ghc/download.html),
[cabal-install](https://www.haskell.org/cabal/download.html)) and install manually, 
or use package management on your operating system if available:
* Mac users can get them via [homebew](//brew.sh/): `brew install ghc cabal-install`.
* Windows users can get them via [chocolatey](//chocolatey.org): `choco install ghc cabal`.
* Ubuntu users are recommended to use [ppa](//launchpad.net/~hvr/+archive/ubuntu/ghc).

## Installation

To use Z-IO package for example, add following lines to your project's cabal file:

```
...
    build-depends:          Z-IO  == 0.1.*
```

Now run `cabal build` within your project directory, cabal should be able to download Z-IO dependency automatically. Let's write a simple tcp echo server for example:

1. Initialize a project with `cabal`.

```
mkdir tcp-echo
cd tcp-echo
cabal init -i
```

Now `cabal` will ask you some simple questions about your project, then create a `tcp-echo.cabal` file automatically, which records the building configuration.

2. Add Z-IO and Z-Data to project's cabal file.

Now open `tcp-echo.cabal` file with a text editor, add following lines under `executable` section:

```
...
    build-depends:          Z-IO  == 0.5.*
                            Z-Data == 0.5.*
```

3. Edit Main.hs.

Open `src/Main.hs` and add a simple echo tcp server:

```


```

You can check [Z-Example](//github.com/haskell-Z/z-example) for some example code.

