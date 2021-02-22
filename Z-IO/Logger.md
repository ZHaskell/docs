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

High performance logging is important to all kinds of applications. In Z-IO all you have to do is to import `Z.IO` and use following functions:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Z.Data.Builder as B
import Z.IO

-- logging functions all directly works in IO monad
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

Note that `debug/info/warning` do not trigger a log flushing, while fatal/critical always trigger a log flushing. If `debug/info/warning` logs matter to you, use `withDefaultLogger` like this:

```
main :: IO
main = withDefaultLogger $ do
    ...
```

It will add a flush after application finishes, to make sure `debug/info/warning` logs are flushed.

# Setup Logger

Z-IO's `Logger` have following concurrent characteristics:

* Logging functions are lock-free, and can be used across threads.
* Logs are atomic, and order are preserved.
* Flushing is protected by lock, there'll be no concurrent writing to buffered device.

So don't have to worry about anything, just import and start to log. There are also `debugTo/infoTo/warningTo...` functions to explictly write logs to given `Logger`, but most of the time you are going to use the default `Logger`. Use `setDefaultLogger` to change it when application starts. Z-IO support writing logs to different device with different format:

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

Initial default logger are connected to stderr, use `defaultColoredFmt` if stderr is connected to a TTY device, and use `defaultFmt` otherwise, an example to setup logger:

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
