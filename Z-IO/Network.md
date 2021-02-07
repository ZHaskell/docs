---
layout: default
parent: Z-IO
title: Network
nav_order: 2
---

Network is all about sending and receiving data, using Z-IO's network is straightforward:

```haskell
import Z.IO
import Z.IO.Network

main :: IO ()
main = do
    -- use getAddrInfo to perform DNS resolution
    addr:_ <- getAddrInfo Nothing "www.bing.com" "http"
    -- use initTCPClient to initialize a TCP client
    withResource (initTCPClient defaultTCPClientConfig{ tcpRemoteAddr = addrAddress addr}) $ \ tcp -> do
        -- use BufferedInput/BufferedOutput facility to read from/write to tcp socket
        i <- newBufferedInput tcp
        o <- newBufferedOutput tcp
        writeBuffer o "GET http://www.bing.com HTTP/1.1\r\nHost: www.bing.com\r\n\r\n"
        flushBuffer o
        readBuffer i >>= pure . T.validate

    -- use startTCPServer to start serving in TCP protocol
    startTCPServer defaultTCPServerConfig{
        tcpListenAddr = SocketAddrIPv4 ipv4Loopback 8080} $ \ tcp -> do
            i <- newBufferedInput tcp
            o <- newBufferedOutput tcp
            forever $ readBuffer i >>= writeBuffer o >> flushBuffer o
```

+ `Z.IO.Network.IPC` provides stream channel for inter-process communication based on domain socket(unix) or named pipe(windows). 
+ `Z.IO.Network.TCP` provides stream channel for remote communication based on TCP socket.
+ `Z.IO.Network.UDP` provides message channel on top of UDP socket.

Z-IO's network modules hide many low-level socket operations, :

```


```
