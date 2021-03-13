{-|
Typical use cases for this module are:

 - binding a packet socket to an interface (combining 'mkBindSockAddrLl' and 'Network.Socket.Address.bind')
 - sending raw packets (combining 'mkSendSockAddrLl' and 'Network.Socket.Address.sendTo')
 - receiving data from a packet socket (using 'Network.Socket.Address.recvFrom')

Here's an example of binding to an interface and receiving packets:

@
main :: IO ()
main = do
  -- Open a raw packet socket, receiving all protocols
  sock <- socket AF_PACKET Raw (toProtocolNumber ETH_P_ALL)

  -- Get the index of the eth0 interface, or fall back to "any interface" (0) if "eth0" doesn't exist
  ifIndex <- fromMaybe 0 \<\$\> ifNameToIndex "eth0"

  -- Bind the socket to the interface index found above
  bind sock (mkBindSockAddrLl AF_PACKET ETH_P_ALL ifIndex)

  -- Print the length of the next 10 packets received on the interface:
  replicateM_ 10 $ do
    msg <- recv sock 4096
    putStrLn $ "Received " ++ (show . BS.length) msg ++ " bytes"

  -- Close the socket
  close sock
@

See [packet (7)](https://man7.org/linux/man-pages/man7/packet.7.html) for more information about packet sockets.
-}
module Network.Socket.Linux (
    -- * sockaddr_ll type
    SockAddrLl
    , sllPktType
    , physicalAddress
    , mkBindSockAddrLl
    , mkSendSockAddrLl
    , IfIndex
    , PhysicalAddress
    , PhysicalAddressBytes
    , addressLength
    , address
    , mkPhysicalAddress
    , macAddress

    -- * ProtocolId
    , ProtocolId(GeneralProtocolId, UnsupportedProtocolId
                ,ETH_P_LOOP,ETH_P_PUP,ETH_P_PUPAT,ETH_P_IP,ETH_P_X25,ETH_P_ARP
                ,ETH_P_BPQ,ETH_P_IEEEPUP,ETH_P_IEEEPUPAT,ETH_P_DEC,ETH_P_DNA_DL
                ,ETH_P_DNA_RC,ETH_P_DNA_RT,ETH_P_LAT,ETH_P_DIAG,ETH_P_CUST
                ,ETH_P_SCA,ETH_P_TEB,ETH_P_RARP,ETH_P_ATALK,ETH_P_AARP
                ,ETH_P_8021Q,ETH_P_IPX,ETH_P_IPV6,ETH_P_PAUSE,ETH_P_SLOW
                ,ETH_P_WCCP,ETH_P_PPP_DISC,ETH_P_PPP_SES,ETH_P_MPLS_UC
                ,ETH_P_MPLS_MC,ETH_P_ATMMPOA,ETH_P_ATMFATE,ETH_P_PAE,ETH_P_AOE
                ,ETH_P_TIPC,ETH_P_1588,ETH_P_FCOE,ETH_P_FIP,ETH_P_EDSA
                ,ETH_P_802_3,ETH_P_AX25,ETH_P_ALL,ETH_P_802_2,ETH_P_SNAP
                ,ETH_P_DDCMP,ETH_P_WAN_PPP,ETH_P_PPP_MP,ETH_P_LOCALTALK
                ,ETH_P_CAN,ETH_P_PPPTALK,ETH_P_TR_802_2,ETH_P_MOBITEX
                ,ETH_P_CONTROL,ETH_P_IRDA,ETH_P_ECONET,ETH_P_HDLC,ETH_P_ARCNET
                ,ETH_P_DSA,ETH_P_TRAILER,ETH_P_PHONET,ETH_P_IEEE802154)
    , isSupportedProtocolId
    , toProtocolNumber

    -- * PacketType
    , PacketType(GeneralPacketType, UnsupportedPacketType
                ,PACKET_HOST,PACKET_BROADCAST,PACKET_MULTICAST,PACKET_OTHERHOST
                ,PACKET_OUTGOING)
    , isSupportedPacketType
    ) where

import Network.Socket.Linux.Types
