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

You need a working Haskell compiler system: GHC(>={{site.data.version.ghc_version}}), cabal-install(>={{site.data.version.cabal_version}}). There are several choices:

+ Use the package manager on your operating system if available:

    * Mac users can get them via [homebew](//brew.sh/): `brew install ghc cabal-install`.
    * Windows users can get them via [chocolatey](//chocolatey.org): `choco install ghc cabal`.
    * Ubuntu users are recommended to use this [ppa](//launchpad.net/~hvr/+archive/ubuntu/ghc).

+ Setup via [ghcup](https://www.haskell.org/ghcup/).

+ Download pre-built binaries([GHC](https://www.haskell.org/ghc/download.html), [cabal-install](https://www.haskell.org/cabal/download.html)) and install manually.

## Installation

To use [Z-Data](https://hackage.haskell.org/package/Z-Data) package as an example. Add the following lines to your project's cabal file:

```
...
    build-depends:          Z-Data == {{site.data.version.z_version}}.*
```

Now run `cabal build` within your project directory, cabal should be able to download [Z-Data](https://hackage.haskell.org/package/Z-Data) dependency automatically. Let's write a simple TCP echo server just for teaching purpose:

1. Initialize a project with `cabal`.

    ```
    mkdir tcp-echo
    cd tcp-echo
    cabal init -i
    ```

    `cabal` will ask you some questions about your project and create a `tcp-echo.cabal` file.

2. Add dependencies.

    Now open the` tcp-echo.cabal` file with a text editor, and add the following lines under the `executable` section:

    ```
    ...
        build-depends:          Z-IO  == {{site.data.version.z_version}}.*
    ```

3. Edit code.

    Open `src/Main.hs` and add a simple echo TCP server:

    ```haskell
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

    Ensure that you have run `cabal update` to get the latest package list. `cabal build` will start to download dependencies and build your project. You may see output like this:

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

    It may take a while to build for the first time because cabal needs to download and build all the dependencies. Build afterward will be faster since dependencies are cached. For reference, on an intel 4th gen core, it takes around 10mins to compile Z-Data and Z-IO. So sit back and relax, or go for a coffee.

After building complete, you can use `cabal run` to run your echo server and `nc 0.0.0.0 8080` to test it. That's it, happy hacking!
