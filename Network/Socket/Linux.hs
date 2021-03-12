module Network.Socket.Linux (
    -- * ProtocolId
    ProtocolId(GeneralProtocolId, UnsupportedProtocolId
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

    -- * sockaddr_ll type
    , SockAddrLl
    , physicalAddress
    , mkBindSockAddrLl
    , mkSendSockAddrLl
    , PhysicalAddress
    , PhysicalAddressBytes
    , addressLength
    , address
    , mkPhysicalAddress
    , macAddress
    ) where

import Network.Socket.Linux.Types
