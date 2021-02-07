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

You need a working haskell compiler system: GHC(>={{site.data.version.ghc_version}}), cabal-install(>={{site.data.version.cabal_version}}). You can download pre-built binaries([GHC](https://www.haskell.org/ghc/download.html),
[cabal-install](https://www.haskell.org/cabal/download.html)) and install manually, 
or use package management on your operating system if available:
* Mac users can get them via [homebew](//brew.sh/): `brew install ghc cabal-install`.
* Windows users can get them via [chocolatey](//chocolatey.org): `choco install ghc cabal`.
* Ubuntu users are recommended to use this [ppa](//launchpad.net/~hvr/+archive/ubuntu/ghc).

## Installation

To use Z-Data package, for example, add the following lines to your project's cabal file:

```
...
    build-depends:          Z-Data == {{site.data.version.z_version}}.*
```

Now run `cabal build` within your project directory, cabal should be able to download Z-Data dependency automatically. Let's write a simple TCP echo server for example:

1. Initialize a project with `cabal`.

    ```
    mkdir tcp-echo
    cd tcp-echo
    cabal init -i
    ```

    Now, `cabal` will ask you some simple questions about your project, then create a `tcp-echo.cabal` file.

2. Add dependencies.

    Now open the` tcp-echo.cabal` file with a text editor, add the following lines under the `executable` section:

    ```
    ...
        build-depends:          Z-IO  == {{site.data.version.z_version}}.*
    ```

3. Edit code.

    Open `src/Main.hs` and add a simple echo TCP server:

    ```
    import Control.Monad
    import Z.IO
    import Z.IO.Network

    main :: IO ()
    main = do
        let addr = SocketAddrIPv4 ipv4Loopback 8080
        startTCPServer defaultTCPServerConfig{ tcpListenAddr = addr } $ \ tcp -> do
            i <- newBufferedInput tcp
            o <- newBufferedOutput tcp
            forever $ readBuffer i >>= writeBuffer o >> flushBuffer o
    ```

4. Build!

    Make sure you have run `cabal update` to get the latest packages' index, then `cabal build` will start to download dependencies and build your project, you may see output like this:

    ```
    Resolving dependencies...
    Build profile: -w ghc-{{site.data.version.ghc_version}} -O1
    In order, the following will be built (use -v for more details):
     - Z-IO-{{site.data.version.z_version}}.0.0 (lib) (requires download & build)
     - tcp-echo-0.1.0.0 (exe:tcp-echo) (first run)
    Downloaded   Z-IO-{{site.data.version.z_version}}.0.0
    Starting     Z-IO-{{site.data.version.z_version}}.0.0 (lib)
    Building     Z-IO-{{site.data.version.z_version}}.0.0 (lib)
    ...
    ```

    It may take a while for the first time build because cabal needs to download and build all the dependencies, building afterward will be faster after dependencies are cached.

Now you can use `cabal run` to run your echo server and `nc 0.0.0.0 8080` to test it, and that's it! happy hacking.
