module Main where

import Control.Monad (replicateM_)
import qualified Data.ByteString as BS
import Data.Maybe

import Network.Socket (Family (AF_PACKET), SocketType (Raw), close, ifNameToIndex, socket)
import Network.Socket.Address
import Network.Socket.ByteString (recv)
import Network.Socket.Linux

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
