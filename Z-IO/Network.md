---
layout: default
parent: Z-IO
title: Network
nav_order: 2
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

# Client and server

Network is all about sending and receiving data, using Z-IO's network is straightforward:

```haskell
import Z.IO
import Z.IO.Network

main :: IO ()
main = do
    -- use getAddrInfo to perform DNS resolution
    addr:_ <- getAddrInfo Nothing "www.bing.com" "http"
    -- use initTCPClient to initialize a TCP client
    withResource (initTCPClient defaultTCPClientConfig{ 
            tcpRemoteAddr = addrAddress addr}) $ \ tcp -> do
        -- use BufferedInput/BufferedOutput facility to read from/write to tcp socket
        i <- newBufferedInput tcp
        o <- newBufferedOutput tcp
        writeBuffer o "GET http://www.bing.com HTTP/1.1\r\nHost: www.bing.com\r\n\r\n"
        flushBuffer o
        readBuffer i >>= pure . T.validate

    -- use startTCPServer to start serving in TCP protocol
    startTCPServer defaultTCPServerConfig{
        tcpListenAddr = SocketAddrIPv4 ipv4Loopback 8080} $ \ tcp -> do
            o <- newBufferedOutput tcp
            writeBuffer o "hello world" >> flushBuffer o
```

Z.Haskell provide several network capabilities:

+ `Z.IO.Network.IPC` provides stream channel for inter-process communication based on domain socket(unix) or named pipe(windows). 
+ `Z.IO.Network.TCP` provides stream channel for remote communication based on TCP socket.
+ `Z.IO.Network.UDP` provides message channel on top of UDP socket.
+ A TLS implementation based on [botan](https://botan.randombit.net/) is under development.

Let's take TCP module as an example, lots of low-level socket details(`bind`, `listen`, `accept`, etc.) are hidden, with two high-level operations left:

```
-- | Connect to a TCP target
initTCPClient :: HasCallStack => TCPClientConfig -> Resource UVStream
-- | Start a TCP server
startTCPServer :: HasCallStack	 
               => TCPServerConfig	 
               -> (UVStream -> IO ())   
               -- ^ worker which will get an accepted TCP stream 
               -- and run in a seperated haskell thread
               -> IO
```

# Send/receive packet

The `UVStream` type implements `Input/Output` class from `Z.IO.Buffered`, so that you can reuse all the buffered read/write API, for example let's say you have designed a simple framed message protocol:

```
import Data.Word
import qualified Z.Data.Vector as V

--   uint8 message type  uint16 payload length  message payload
--  +------------------+----------------------+------------------
--  |       0xXX       |   0xXXXX(big endian) |       ...
--  +------------------+----------------------+------------------

data Message = Message { msgTyp :: Word8, msgPayload :: V.Bytes }
```

You can manually decode message frames like this:

```haskell
-- import bit operations
import Data.Bits    (unsafeShiftL, (.|.))
import Z.IO

readMessage :: HasCallStack => BufferedInput -> IO Message
readMessage bi = do
    msg_typ <- readExactly buffered_i 1
    payload_len_h <- readExactly buffered_i 1
    payload_len_l <- readExactly buffered_i 1
    let payload_len =
        (fromIntegral payload_len_h) `unsafeShiftL` 8 
            .|. (fromIntegral payload_len_l)
    payload <- readExactly payload_len 
    return (Message msg_typ payload)
```

Or you can use `Parser` from `Z.Data.Parser` module:


```haskell
import qualified Z.Data.Parser as P
import Data.Word
import Z.IO

parseMessage :: P.Parser Message
parseMessage = do
    msg_type <- P.decodePrim @Word8
    payload_len <- P.decodePrimBE @Word16
    payload <- P.take (fromIntegral payload_len)
    return (Message msg_typ payload)

readMessage :: HasCallStack => BufferedInput -> IO Message
readMessage = readParser parseMessage
```

`readParser` will run `Parser` once a time, parse `Message` out of buffer, and waiting for input automatically. To write a `Message` to TCP socket is similar:

```haskell
import qualified Z.Data.Builder as B
import qualified Z.Data.Vector as V
import Z.IO

writeMessage :: HasCallStack => BufferedOutput -> Message -> IO ()
writeMessage bo (Message msg_typ payload) = do
    -- use Builder monad to compose buffer writing functions
    writeBuilder bo $ do
        B.encodePrim msg_typ
        B.encodePrimBE (V.length payload)
        B.bytes payload
    -- you may want to add a flush after each message has been written  
    -- or leave flush to the caller
    -- flushBuffer bo
``` 

Z.Haskell provides many tools to deal with the streaming nature of TCP protocol(and many other streaming devices such as IPC and Files). In the next section, we will introduce the `BIO`, a more high-level streaming API.

# UDP

UDP is differet from IPC or TCP in that it's a message protocol rather than a streaming one. There're no `Input/Output` instances for `UDP` type. Instead, Z-IO directly provide message reading & writing functions for UDP:

```haskell
-- | Initialize a UDP socket.
initUDP :: UDPConfig -> Resource UDP
-- | Send a UDP message to target address.
sendUDP :: HasCallStack => UDP -> SocketAddr -> V.Bytes -> IO ()
-- | Receive messages from UDP socket, return source address if available, and a `Bool`
-- to indicate if the message is partial (larger than receive buffer size).
recvUDP :: HasCallStack => UDPRecvConfig -> UDP -> IO [(Maybe SocketAddr, Bool, V.Bytes)]
-- | Receive UDP messages within a loop
recvUDPLoop :: HasCallStack
            => UDPRecvConfig
            -> UDP
            -> ((Maybe SocketAddr, Bool, V.Bytes) -> IO a)
            -> IO ()
```

Loop receiving(`recvUDPLoop`) can be faster since it can reuse the receiving buffer internally. Unlike TCP server above, UDP worker function is called on current haskell thread instead of a forked one. If you have heavy computations to do within the worker function, consider using 'forkBa' from `Z.IO.UV.Manager`(a function similar to `forkIO`, but with active thread balancing, or a producer-consumer architecture.

