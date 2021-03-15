---
layout: default
parent: Z-IO
title: Logger
nav_order: 4
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

# Logging functions

High-performance logging is important to all kinds of applications. In Z-IO, all you have to do is to import `Z.IO` and use the following functions:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Z.Data.Builder as B
import Z.IO

-- logging functions all work directly in IO monad
debug, info , warning, fatal, critical :: B.Builder () -> IO ()

-- you can use B.Builder's IsString instance
debug "..."
-- together with B.Builder's Monad instance 
info $ "..." >> B.int 666 >> "..."
warning $ do
    "..."
    B.int 666
    "..."
fatal "this is an important message"
critical "OMG, system is on fire"
```

Note that `debug/info/warning` does not trigger a log flushing, while fatal/critical always triggers a log flushing. If `debug/info/warning` logs matter to you, use `withDefaultLogger` like this:

```
main :: IO
main = withDefaultLogger $ do
    ...
```

It will add a flush after the application finishes to ensure that `debug/info/warning` logs are flushed.

# Setup Logger

Z-IO's `Logger` have the following concurrent characteristics:

* Logging functions are lock-free and can be used across threads.
* Logs are atomic, and the order is preserved.
* Flushing is protected by the lock, and there'll be no concurrent writing to the buffered device.

So there is no need to worry about anything since most of the things are taken care of, just import and start to log. Functions like `debugTo/infoTo/warningTo...` that explicitly write logs to given `Logger` are provided. However, most of the time, use the default `Logger`. And, use `setDefaultLogger` to change it when the application starts. Z-IO supports writing logs to different devices with different formats:

```haskell
-- logs can be written to any `BufferedOutput`s with `MVar` protected from concurrent access
newLogger :: LoggerConfig -> MVar BufferedOutput -> IO Logger
-- create a logger connected to stderr
newStdLogger :: LoggerConfig -> IO Logger
-- create a file based logger 
newFileLogger :: LoggerConfig -> CBytes -> IO Logger

-- Change LoggerConfig's loggerFormatter field to change logging format:
-- [FATAL][2021-02-01T15:03:30+0800][interactive:31:1][thread#669]...\n
defaultFmt :: LogFormatter
-- Same with defaultFmt, but level is colored: cyan DEBUG, yellow WARNING, red FATAL and CRITICAL
defaultColoredFmt :: LogFormatter
-- {"level":"FATAL","time":"2021-02-01T15:02:19+0800","loc":"<interactive>:27:1","theadId":606,"content":"..."}\n
defaultJSONFmt :: LogFormatter
```

Initial default loggers are connected to stderr. Use `defaultColoredFmt` if stderr is connected to a TTY device, and use `defaultFmt` otherwise. An example about setting up logger:

```haskell
main :: IO ()
main = do
    -- setup filter level to WARNING, info/debug logs will be ignored.
    -- use file based logger, and write to "app.log"
    setDefaultLogger =<< newFileLogger defaultJSONLoggerConfig
        { loggerConfigLevel = WARNING } "app.log"
    withDefaultLogger $ do
        ...
```
