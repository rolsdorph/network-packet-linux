# network-packet-linux
This package provides types that make it possible to use the `network` package with Linux packet sockets. Packet sockets are described in [packet (7)](https://man7.org/linux/man-pages/man7/packet.7.html).

As of version 3.0.0, the [network](https://hackage.haskell.org/package/network) package provides a [socket API](https://hackage.haskell.org/package/network/docs/Network-Socket-Address.html#g:2) that is 
implemented in terms of the `SocketAddress` typeclass. `network-packet-linux` provides a `SocketAddress` instance named `SockAddrLl`, which represents a `sockaddr_ll`.

## Usage
See [examples/ListenToInterface.hs](examples/ListenToInterface.hs) for the full example file. 

```
main :: IO ()
main = do
  -- Open a raw packet socket, receiving all protocols
  sock <- socket AF_PACKET Raw (toProtocolNumber ETH_P_ALL)

  -- Get the index of the eth0 interface, fall back to "any interface" (0) if
  -- "eth0" doesn't exist
  ifIndex <- fromMaybe 0 <$> ifNameToIndex "eth0"

  -- Bind the socket to the interface index found above
  bind sock (mkBindSockAddrLl AF_PACKET ETH_P_ALL ifIndex)

  -- Print the length of the next 10 packets received on the interface:
  replicateM_ 10 $ do
    msg <- recv sock 4096
    putStrLn $ "Received " ++ (show . BS.length) msg ++ " bytes"

  -- Close the socket
  close sock
```

## About this package
The code in the first version of `network-packet-linux` was initially written as part of the `network` package, and later extracted to a separate package. This separation was done because `network` aims to mostly contain cross-platform functionality. 
This package therefore looks a lot like its parent - code style, build setup and tests look very much like in `network`. The repo even contains [network](https://github.com/haskell/network/) as a git submodule, in order to re-use some of its code.

## Building
If you didn't include the submodules when cloning the repository, run `git submodule init` and `git submodule update` to fetch the necessary files from `network`.

Then, to build: `autoreconf -i`, `cabal configure` and finally `cabal build`

## Tests
There are two test suites, residing in the `test-socket` and `test-types` directories. The latter is for "normal" (purely functional) tests.

`test-socket` is a bit weirder. It contains end-to-end tests of the socket functionality. In order to verify that we're actually able to bind a socket to an interface, it needs an
an interface named `haskellnetwork` (see `setup_unixtest_interface.sh` and `teardown_unixtest_interface.sh` for helper scripts that create/remove such an interface).

As if that wasn't annoying enough, it tests the packet socket functionality. In order to interact with `AF_PACKET` sockets, the test executable needs the `CAP_NET_RAW` capability - that is, it needs to *run as root*. I recommend using a namespace for this: [run_socketspec_namespaced.sh](run_socketspec_namespaced.sh) is a helper script that performs the necessary setup and executes the socket tests inside a namespace.
