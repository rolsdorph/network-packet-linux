{-# LANGUAGE PatternSynonyms #-}

#include "LinuxDef.h"

module Network.Socket.Linux.Types (
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

import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Network.Socket.ReadShow
import Network.Socket (Family, packFamily, unpackFamily)
import Network.Socket.Address (SocketAddress, peekSocketAddress, pokeSocketAddress, sizeOfSocketAddress)
import Text.Read (readPrec)

foreign import ccall unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import ccall unsafe "htons" htons :: Word16 -> Word16
foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()

------------------------------------------------------------------------
-- | Ethernet Protocol IDs.
--
-- These are IEEE 802.3 protocol numbers (as defined in @\<linux/if_ether.h>@),
-- for use in 'SockAddrLl' addresses when working with packet sockets.
--
-- Some of the defined patterns may be unsupported on some systems:
-- see 'isSupportedProtocolId'.
newtype ProtocolId = ProtocolId { packProtocolId :: CUShort } deriving (Eq, Ord)

-- | Does one of the @ETH_@ constants correspond to a known Ethernet protocol id
-- on this system?
--
-- Just like for 'Network.Socket.isSupportedFamily', 'GeneralProtocolId' values
-- not equal to any of the named @ETH_xxxxx@ patterns or 'UnsupportedProtocolId' 
-- will return 'True' even when not known on this system.
isSupportedProtocolId :: ProtocolId -> Bool
isSupportedProtocolId f = case f of
    UnsupportedProtocolId -> False
    GeneralProtocolId _   -> True

pattern GeneralProtocolId  :: CUShort -> ProtocolId
pattern GeneralProtocolId n = ProtocolId n
#if __GLASGOW_HASKELL__ >= 806
{-# COMPLETE GeneralProtocolId #-}
#endif

-- | Convert 'CUShort' to 'ProtocolId'.
unpackProtocolId :: CUShort -> ProtocolId
unpackProtocolId = ProtocolId
{-# INLINE unpackProtocolId #-}

-- | Unsupported protocol id, equal to any other protocol ids that are not
-- supported on the system.
pattern UnsupportedProtocolId  :: ProtocolId
pattern UnsupportedProtocolId   = ProtocolId (-1)

-- | Ethernet Loopback packet
pattern ETH_P_LOOP             :: ProtocolId
#ifdef ETH_P_LOOP
pattern ETH_P_LOOP              = ProtocolId (#const ETH_P_LOOP)
#else
pattern ETH_P_LOOP = ProtocolId (-1)
#endif

-- | Xerox PUP packet
pattern ETH_P_PUP              :: ProtocolId
#ifdef ETH_P_PUP
pattern ETH_P_PUP               = ProtocolId (#const ETH_P_PUP)
#else
pattern ETH_P_PUP               = ProtocolId (-1)
#endif

-- | Xerox PUP Addr Trans packet
pattern ETH_P_PUPAT            :: ProtocolId
#ifdef ETH_P_PUPAT
pattern ETH_P_PUPAT             = ProtocolId (#const ETH_P_PUPAT)
#else
pattern ETH_P_PUPAT             = ProtocolId (-1)
#endif

-- | Internet Protocol packet
pattern ETH_P_IP               :: ProtocolId
#ifdef ETH_P_IP
pattern ETH_P_IP                = ProtocolId (#const ETH_P_IP)
#else
pattern ETH_P_IP                = ProtocolId (-1)
#endif

-- | CCITT X.25
pattern ETH_P_X25              :: ProtocolId
#ifdef ETH_P_X25
pattern ETH_P_X25               = ProtocolId (#const ETH_P_X25)
#else
pattern ETH_P_X25               = ProtocolId (-1)
#endif

-- | Address Resolution packet
pattern ETH_P_ARP              :: ProtocolId
#ifdef ETH_P_ARP
pattern ETH_P_ARP               = ProtocolId (#const ETH_P_ARP)
#else
pattern ETH_P_ARP               = ProtocolId (-1)
#endif

-- | G8BPQ AX.25 Ethernet Packet [ NOT AN OFFICIALLY REGISTERED ID ]
pattern ETH_P_BPQ              :: ProtocolId
#ifdef ETH_P_BPQ
pattern ETH_P_BPQ               = ProtocolId (#const ETH_P_BPQ)
#else
pattern ETH_P_BPQ               = ProtocolId (-1)
#endif

-- | Xerox IEEE802.3 PUP packet
pattern ETH_P_IEEEPUP          :: ProtocolId
#ifdef ETH_P_IEEEPUP
pattern ETH_P_IEEEPUP           = ProtocolId (#const ETH_P_IEEEPUP)
#else
pattern ETH_P_IEEEPUP           = ProtocolId (-1)
#endif

-- | Xerox IEEE802.3 PUP Addr Trans packet
pattern ETH_P_IEEEPUPAT        :: ProtocolId
#ifdef ETH_P_IEEEPUPAT
pattern ETH_P_IEEEPUPAT         = ProtocolId (#const ETH_P_IEEEPUPAT)
#else
pattern ETH_P_IEEEPUPAT         = ProtocolId (-1)
#endif

-- | DEC Assigned proto
pattern ETH_P_DEC              :: ProtocolId
#ifdef ETH_P_DEC
pattern ETH_P_DEC               = ProtocolId (#const ETH_P_DEC)
#else
pattern ETH_P_DEC               = ProtocolId (-1)
#endif

-- | DEC DNA Dump/Load
pattern ETH_P_DNA_DL           :: ProtocolId
#ifdef ETH_P_DNA_DL
pattern ETH_P_DNA_DL            = ProtocolId (#const ETH_P_DNA_DL)
#else
pattern ETH_P_DNA_DL            = ProtocolId (-1)
#endif

-- | DEC DNA Remote Console
pattern ETH_P_DNA_RC           :: ProtocolId
#ifdef ETH_P_DNA_RC
pattern ETH_P_DNA_RC            = ProtocolId (#const ETH_P_DNA_RC)
#else
pattern ETH_P_DNA_RC            = ProtocolId (-1)
#endif

-- | DEC DNA Routing
pattern ETH_P_DNA_RT           :: ProtocolId
#ifdef ETH_P_DNA_RT
pattern ETH_P_DNA_RT            = ProtocolId (#const ETH_P_DNA_RT)
#else
pattern ETH_P_DNA_RT            = ProtocolId (-1)
#endif

-- | DEC LAT
pattern ETH_P_LAT              :: ProtocolId
#ifdef ETH_P_LAT
pattern ETH_P_LAT               = ProtocolId (#const ETH_P_LAT)
#else
pattern ETH_P_LAT               = ProtocolId (-1)
#endif

-- | DEC Diagnostics
pattern ETH_P_DIAG             :: ProtocolId
#ifdef ETH_P_DIAG
pattern ETH_P_DIAG              = ProtocolId (#const ETH_P_DIAG)
#else
pattern ETH_P_DIAG              = ProtocolId (-1)
#endif

-- | DEC Customer use
pattern ETH_P_CUST             :: ProtocolId
#ifdef ETH_P_CUST
pattern ETH_P_CUST              = ProtocolId (#const ETH_P_CUST)
#else
pattern ETH_P_CUST              = ProtocolId (-1)
#endif

-- | DEC Systems Comms Arch
pattern ETH_P_SCA              :: ProtocolId
#ifdef ETH_P_SCA
pattern ETH_P_SCA               = ProtocolId (#const ETH_P_SCA)
#else
pattern ETH_P_SCA               = ProtocolId (-1)
#endif

-- | Trans Ether Bridging
pattern ETH_P_TEB              :: ProtocolId
#ifdef ETH_P_TEB
pattern ETH_P_TEB               = ProtocolId (#const ETH_P_TEB)
#else
pattern ETH_P_TEB               = ProtocolId (-1)
#endif

-- | Reverse Addr Res packet
pattern ETH_P_RARP             :: ProtocolId
#ifdef ETH_P_RARP
pattern ETH_P_RARP              = ProtocolId (#const ETH_P_RARP)
#else
pattern ETH_P_RARP              = ProtocolId (-1)
#endif

-- | Appletalk DDP
pattern ETH_P_ATALK            :: ProtocolId
#ifdef ETH_P_ATALK
pattern ETH_P_ATALK             = ProtocolId (#const ETH_P_ATALK)
#else
pattern ETH_P_ATALK             = ProtocolId (-1)
#endif

-- | Appletalk AARP
pattern ETH_P_AARP             :: ProtocolId
#ifdef ETH_P_AARP
pattern ETH_P_AARP              = ProtocolId (#const ETH_P_AARP)
#else
pattern ETH_P_AARP              = ProtocolId (-1)
#endif

-- | 802\.1Q VLAN Extended Header
pattern ETH_P_8021Q            :: ProtocolId
#ifdef ETH_P_8021Q
pattern ETH_P_8021Q             = ProtocolId (#const ETH_P_8021Q)
#else
pattern ETH_P_8021Q             = ProtocolId (-1)
#endif

-- | IPX over DIX
pattern ETH_P_IPX              :: ProtocolId
#ifdef ETH_P_IPX
pattern ETH_P_IPX               = ProtocolId (#const ETH_P_IPX)
#else
pattern ETH_P_IPX               = ProtocolId (-1)
#endif

-- | IPv6 over bluebook
pattern ETH_P_IPV6             :: ProtocolId
#ifdef ETH_P_IPV6
pattern ETH_P_IPV6              = ProtocolId (#const ETH_P_IPV6)
#else
pattern ETH_P_IPV6              = ProtocolId (-1)
#endif

-- | IEEE Pause frames. See 802.3 31B
pattern ETH_P_PAUSE            :: ProtocolId
#ifdef ETH_P_PAUSE
pattern ETH_P_PAUSE             = ProtocolId (#const ETH_P_PAUSE)
#else
pattern ETH_P_PAUSE             = ProtocolId (-1)
#endif

-- | Slow Protocol. See 802.3ad 43B
pattern ETH_P_SLOW             :: ProtocolId
#ifdef ETH_P_SLOW
pattern ETH_P_SLOW              = ProtocolId (#const ETH_P_SLOW)
#else
pattern ETH_P_SLOW              = ProtocolId (-1)
#endif

-- | Web-cache coordination protocol
pattern ETH_P_WCCP             :: ProtocolId
#ifdef ETH_P_WCCP
pattern ETH_P_WCCP              = ProtocolId (#const ETH_P_WCCP)
#else
pattern ETH_P_WCCP              = ProtocolId (-1)
#endif

-- | PPPoE discovery messages
pattern ETH_P_PPP_DISC         :: ProtocolId
#ifdef ETH_P_PPP_DISC
pattern ETH_P_PPP_DISC          = ProtocolId (#const ETH_P_PPP_DISC)
#else
pattern ETH_P_PPP_DISC          = ProtocolId (-1)
#endif

-- | PPPoE session messages
pattern ETH_P_PPP_SES          :: ProtocolId
#ifdef ETH_P_PPP_SES
pattern ETH_P_PPP_SES           = ProtocolId (#const ETH_P_PPP_SES)
#else
pattern ETH_P_PPP_SES           = ProtocolId (-1)
#endif

-- | MPLS Unicast traffic
pattern ETH_P_MPLS_UC          :: ProtocolId
#ifdef ETH_P_MPLS_UC
pattern ETH_P_MPLS_UC           = ProtocolId (#const ETH_P_MPLS_UC)
#else
pattern ETH_P_MPLS_UC           = ProtocolId (-1)
#endif

-- | MPLS Multicast traffic
pattern ETH_P_MPLS_MC          :: ProtocolId
#ifdef ETH_P_MPLS_MC
pattern ETH_P_MPLS_MC           = ProtocolId (#const ETH_P_MPLS_MC)
#else
pattern ETH_P_MPLS_MC           = ProtocolId (-1)
#endif

-- | MultiProtocol Over ATM
pattern ETH_P_ATMMPOA          :: ProtocolId
#ifdef ETH_P_ATMMPOA
pattern ETH_P_ATMMPOA           = ProtocolId (#const ETH_P_ATMMPOA)
#else
pattern ETH_P_ATMMPOA           = ProtocolId (-1)
#endif

-- | Frame-based ATM Transport
pattern ETH_P_ATMFATE          :: ProtocolId
#ifdef ETH_P_ATMFATE
pattern ETH_P_ATMFATE           = ProtocolId (#const ETH_P_ATMFATE)
#else
pattern ETH_P_ATMFATE           = ProtocolId (-1)
#endif

-- | Port Access Entity (IEEE 802.1X)
pattern ETH_P_PAE              :: ProtocolId
#ifdef ETH_P_PAE
pattern ETH_P_PAE               = ProtocolId (#const ETH_P_PAE)
#else
pattern ETH_P_PAE               = ProtocolId (-1)
#endif

-- | ATA over Ethernet
pattern ETH_P_AOE              :: ProtocolId
#ifdef ETH_P_AOE
pattern ETH_P_AOE               = ProtocolId (#const ETH_P_AOE)
#else
pattern ETH_P_AOE               = ProtocolId (-1)
#endif

-- | TIPC
pattern ETH_P_TIPC             :: ProtocolId
#ifdef ETH_P_TIPC
pattern ETH_P_TIPC              = ProtocolId (#const ETH_P_TIPC)
#else
pattern ETH_P_TIPC              = ProtocolId (-1)
#endif

-- | IEEE 1588 Timesync
pattern ETH_P_1588             :: ProtocolId
#ifdef ETH_P_1588
pattern ETH_P_1588              = ProtocolId (#const ETH_P_1588)
#else
pattern ETH_P_1588              = ProtocolId (-1)
#endif

-- | Fibre Channel over Ethernet
pattern ETH_P_FCOE             :: ProtocolId
#ifdef ETH_P_FCOE
pattern ETH_P_FCOE              = ProtocolId (#const ETH_P_FCOE)
#else
pattern ETH_P_FCOE              = ProtocolId (-1)
#endif

-- | FCoE Initialization Protocol
pattern ETH_P_FIP              :: ProtocolId
#ifdef ETH_P_FIP
pattern ETH_P_FIP               = ProtocolId (#const ETH_P_FIP)
#else
pattern ETH_P_FIP               = ProtocolId (-1)
#endif

-- | Ethertype DSA [ NOT AN OFFICIALLY REGISTERED ID ]
pattern ETH_P_EDSA             :: ProtocolId
#ifdef ETH_P_EDSA
pattern ETH_P_EDSA              = ProtocolId (#const ETH_P_EDSA)
#else
pattern ETH_P_EDSA              = ProtocolId (-1)
#endif

-- | Dummy type for 802.3 frames
pattern ETH_P_802_3            :: ProtocolId
#ifdef ETH_P_802_3
pattern ETH_P_802_3             = ProtocolId (#const ETH_P_802_3)
#else
pattern ETH_P_802_3             = ProtocolId (-1)
#endif

-- | Dummy protocol id for AX\.25
pattern ETH_P_AX25             :: ProtocolId
#ifdef ETH_P_AX25
pattern ETH_P_AX25              = ProtocolId (#const ETH_P_AX25)
#else
pattern ETH_P_AX25              = ProtocolId (-1)
#endif

-- | Every packet (be careful!!!)
pattern ETH_P_ALL              :: ProtocolId
#ifdef ETH_P_ALL
pattern ETH_P_ALL               = ProtocolId (#const ETH_P_ALL)
#else
pattern ETH_P_ALL               = ProtocolId (-1)
#endif

-- | 802\.2 frames
pattern ETH_P_802_2            :: ProtocolId
#ifdef ETH_P_802_2
pattern ETH_P_802_2             = ProtocolId (#const ETH_P_802_2)
#else
pattern ETH_P_802_2             = ProtocolId (-1)
#endif

-- | Internal only
pattern ETH_P_SNAP             :: ProtocolId
#ifdef ETH_P_SNAP
pattern ETH_P_SNAP              = ProtocolId (#const ETH_P_SNAP)
#else
pattern ETH_P_SNAP              = ProtocolId (-1)
#endif

-- | DEC DDCMP: Internal only
pattern ETH_P_DDCMP            :: ProtocolId
#ifdef ETH_P_DDCMP
pattern ETH_P_DDCMP             = ProtocolId (#const ETH_P_DDCMP)
#else
pattern ETH_P_DDCMP             = ProtocolId (-1)
#endif

-- | Dummy type for WAN PPP frames
pattern ETH_P_WAN_PPP          :: ProtocolId
#ifdef ETH_P_WAN_PPP
pattern ETH_P_WAN_PPP           = ProtocolId (#const ETH_P_WAN_PPP)
#else
pattern ETH_P_WAN_PPP           = ProtocolId (-1)
#endif

-- | Dummy type for PPP MP frames
pattern ETH_P_PPP_MP           :: ProtocolId
#ifdef ETH_P_PPP_MP
pattern ETH_P_PPP_MP            = ProtocolId (#const ETH_P_PPP_MP)
#else
pattern ETH_P_PPP_MP            = ProtocolId (-1)
#endif

-- | Localtalk pseudo type
pattern ETH_P_LOCALTALK        :: ProtocolId
#ifdef ETH_P_LOCALTALK
pattern ETH_P_LOCALTALK         = ProtocolId (#const ETH_P_LOCALTALK)
#else
pattern ETH_P_LOCALTALK         = ProtocolId (-1)
#endif

-- | Controller Area Network
pattern ETH_P_CAN              :: ProtocolId
#ifdef ETH_P_CAN
pattern ETH_P_CAN               = ProtocolId (#const ETH_P_CAN)
#else
pattern ETH_P_CAN               = ProtocolId (-1)
#endif

-- | Dummy type for Atalk over PPP
pattern ETH_P_PPPTALK          :: ProtocolId
#ifdef ETH_P_PPPTALK
pattern ETH_P_PPPTALK           = ProtocolId (#const ETH_P_PPPTALK)
#else
pattern ETH_P_PPPTALK           = ProtocolId (-1)
#endif

-- | 802\.2 frames
pattern ETH_P_TR_802_2         :: ProtocolId
#ifdef ETH_P_TR_802_2
pattern ETH_P_TR_802_2          = ProtocolId (#const ETH_P_TR_802_2)
#else
pattern ETH_P_TR_802_2          = ProtocolId (-1)
#endif

-- | Mobitex (kaz@cafe.net)
pattern ETH_P_MOBITEX          :: ProtocolId
#ifdef ETH_P_MOBITEX
pattern ETH_P_MOBITEX           = ProtocolId (#const ETH_P_MOBITEX)
#else
pattern ETH_P_MOBITEX           = ProtocolId (-1)
#endif

-- | Card specific control frames
pattern ETH_P_CONTROL          :: ProtocolId
#ifdef ETH_P_CONTROL
pattern ETH_P_CONTROL           = ProtocolId (#const ETH_P_CONTROL)
#else
pattern ETH_P_CONTROL           = ProtocolId (-1)
#endif

-- | Linux-IrDA
pattern ETH_P_IRDA             :: ProtocolId
#ifdef ETH_P_IRDA
pattern ETH_P_IRDA              = ProtocolId (#const ETH_P_IRDA)
#else
pattern ETH_P_IRDA              = ProtocolId (-1)
#endif

-- | Acorn Econet
pattern ETH_P_ECONET           :: ProtocolId
#ifdef ETH_P_ECONET
pattern ETH_P_ECONET            = ProtocolId (#const ETH_P_ECONET)
#else
pattern ETH_P_ECONET            = ProtocolId (-1)
#endif

-- | HDLC frames
pattern ETH_P_HDLC             :: ProtocolId
#ifdef ETH_P_HDLC
pattern ETH_P_HDLC              = ProtocolId (#const ETH_P_HDLC)
#else
pattern ETH_P_HDLC              = ProtocolId (-1)
#endif

-- | 1A for ArcNet :-)
pattern ETH_P_ARCNET           :: ProtocolId
#ifdef ETH_P_ARCNET
pattern ETH_P_ARCNET            = ProtocolId (#const ETH_P_ARCNET)
#else
pattern ETH_P_ARCNET            = ProtocolId (-1)
#endif

-- | Distributed Switch Arch.
pattern ETH_P_DSA              :: ProtocolId
#ifdef ETH_P_DSA
pattern ETH_P_DSA               = ProtocolId (#const ETH_P_DSA)
#else
pattern ETH_P_DSA               = ProtocolId (-1)
#endif

-- | Trailer switch tagging
pattern ETH_P_TRAILER          :: ProtocolId
#ifdef ETH_P_TRAILER
pattern ETH_P_TRAILER           = ProtocolId (#const ETH_P_TRAILER)
#else
pattern ETH_P_TRAILER           = ProtocolId (-1)
#endif

-- | Nokia Phonet frames
pattern ETH_P_PHONET           :: ProtocolId
#ifdef ETH_P_PHONET
pattern ETH_P_PHONET            = ProtocolId (#const ETH_P_PHONET)
#else
pattern ETH_P_PHONET            = ProtocolId (-1)
#endif

-- | IEEE802.15.4 frame
pattern ETH_P_IEEE802154       :: ProtocolId
#ifdef ETH_P_IEEE802154
pattern ETH_P_IEEE802154        = ProtocolId (#const ETH_P_IEEE802154)
#else
pattern ETH_P_IEEE802154        = ProtocolId (-1)
#endif

------------------------------------------------------------------------
-- | Packet Types.
--
-- Linux packet types as defined in @\<linux/if_packet.h>@, for use in
-- SockAddrLl addresses when working with packet sockets.
--
-- Some of the defined patterns may be unsupported on some systems:
-- see 'isSupportedPacketType'.
newtype PacketType = PacketType { packPacketType :: CUChar } deriving (Eq, Ord)

-- | Does one of the @PACKET_@ constants correspond to a known packet type
-- on this system?
--
-- Like 'Network.Socket.isSupportedFamily', 'GeneralPacketType' values
-- not equal to any of the named @PACKET_@ patterns or 'UnsupportedPacketType'
-- will return 'True' even when not known on this system.
isSupportedPacketType :: PacketType -> Bool
isSupportedPacketType f = case f of
    UnsupportedPacketType -> False
    GeneralPacketType _   -> True

pattern GeneralPacketType :: CUChar -> PacketType
pattern GeneralPacketType n = PacketType n
#if __GLASGOW_HASKELL__ >= 806
{-# COMPLETE GeneralPacketType #-}
#endif

-- | Convert 'CUChar' to 'PacketType'.
unpackPacketType :: CUChar -> PacketType
unpackPacketType = PacketType
{-# INLINE unpackPacketType #-}

-- | Unsupported packet id, equal to any other packet ids that are not
-- supported on the system.
pattern UnsupportedPacketType  :: PacketType
pattern UnsupportedPacketType   = PacketType (-1)

-- | To us
pattern PACKET_HOST            :: PacketType
#ifdef PACKET_HOST
pattern PACKET_HOST             = PacketType (#const PACKET_HOST)
#else
pattern PACKET_HOST             = PacketType (-1)
#endif

-- | To all
pattern PACKET_BROADCAST       :: PacketType
#ifdef PACKET_BROADCAST
pattern PACKET_BROADCAST        = PacketType (#const PACKET_BROADCAST)
#else
pattern PACKET_BROADCAST        = PacketType (-1)
#endif

-- | To group
pattern PACKET_MULTICAST       :: PacketType
#ifdef PACKET_MULTICAST
pattern PACKET_MULTICAST        = PacketType (#const PACKET_MULTICAST)
#else
pattern PACKET_MULTICAST        = PacketType (-1)
#endif

-- | To someone else
pattern PACKET_OTHERHOST       :: PacketType
#ifdef PACKET_OTHERHOST
pattern PACKET_OTHERHOST        = PacketType (#const PACKET_OTHERHOST)
#else
pattern PACKET_OTHERHOST        = PacketType (-1)
#endif

-- | Outgoing of any type
pattern PACKET_OUTGOING        :: PacketType
#ifdef PACKET_OUTGOING
pattern PACKET_OUTGOING         = PacketType (#const PACKET_OUTGOING)
#else
pattern PACKET_OUTGOING         = PacketType (-1)
#endif
------------------------------------------------------------------------
-- | A physical address consisting of up to eight bytes.
--
-- This type encapsulates the @sll_halen@ and @sll_addr@ fields of
-- @sockaddr_ll@ (as defined in @\<linux/if_packet.h>@).
data PhysicalAddress = PhysicalAddress { addressLength :: SllHaLen 
                                         -- ^ Get the length of the given 'PhysicalAddress'.
                                       , sllAddr :: SllAddr }
    deriving (Show, Eq)

-- | Up to eight bytes, for use with a 'PhysicalAddress'
type PhysicalAddressBytes = (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)

-- | Get the address bytes for the given 'PhysicalAddress'
address :: PhysicalAddress -> PhysicalAddressBytes
address addr = bytes where
                 (SllAddr bytes) = sllAddr addr

-- | Create a new 'PhysicalAddress' with the given length and bytes.
-- Note that 'addressLength' must be between 0 and 8
mkPhysicalAddress :: Int -> PhysicalAddressBytes -> Maybe PhysicalAddress
mkPhysicalAddress haLen addr
    | haLen >= 0 && haLen <= 8 = Just $ PhysicalAddress { addressLength = fromIntegral haLen
                                                        , sllAddr = SllAddr addr }
    | otherwise = Nothing

-- | Create a new 'PhysicalAddress' to represent the the given MAC address.
macAddress :: (Word8, Word8, Word8, Word8, Word8, Word8) -> PhysicalAddress
macAddress (a,b,c,d,e,f) = PhysicalAddress { addressLength = 6
                                           , sllAddr = SllAddr (a,b,c,d,e,f,0,0) }

emptyAddress :: PhysicalAddress
emptyAddress = PhysicalAddress { addressLength = 0, sllAddr = SllAddr (0,0,0,0,0,0,0,0) }

-- | A type representing the @sockaddr_ll@ struct defined in
-- @\<linux/if_ether.h>@.
--
-- This can be used by functions in "Network.Socket.Address" in order to
-- interact with packet sockets - see
-- [packet (7)](https://man7.org/linux/man-pages/man7/packet.7.html) for
-- details. Note that passing this type to 'Network.Socket.Address.connect' or
-- 'Network.Socket.Address.accept' is an error.
data SockAddrLl = SockAddrLl { sllFamily :: Family
                             , sllProtocol :: ProtocolId
                             , sllIfIndex :: SllIfIndex
                             , sllHaType :: SllHaType
                             , sllPktType :: PacketType
                             , physicalAddress :: PhysicalAddress -- ^ Get the 'PhysicalAddress' corresponding to the the @sll_halen@ and @sll_addr@ fields
                             }

-- | Create a SockAddrLl for binding a packet socket to an interface.
mkBindSockAddrLl :: Family -> ProtocolId -> Int -> SockAddrLl
mkBindSockAddrLl family protocol index =
    SockAddrLl { sllFamily = family
               , sllProtocol = protocol
               , sllIfIndex = fromIntegral index
               , sllHaType = 0
               , sllPktType = PacketType 0
               , physicalAddress = emptyAddress }

-- | Create a SockAddrLl for sending data to a specific address.
mkSendSockAddrLl :: Family -> ProtocolId -> Int -> PhysicalAddress -> SockAddrLl
mkSendSockAddrLl family protocol index addr =
    SockAddrLl { sllFamily = family
               , sllProtocol = protocol
               , sllIfIndex = (fromIntegral index)
               , sllHaType = 0
               , sllPktType = (PacketType 0)
               , physicalAddress = addr }

instance SocketAddress SockAddrLl where
    sizeOfSocketAddress = sizeOfSockAddrLl
    peekSocketAddress   = peekSockAddrLl
    pokeSocketAddress   = pokeSockAddrLl

-- Internal representation of the bytes in a SockAddrLl, needed so we can create
-- an instance of Storable
newtype SllAddr = SllAddr (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) deriving (Show, Eq)

instance Storable SllAddr where
    sizeOf    _ = 8
    alignment _ = 0

    peek p = do
        a <- peekByteOff p 0
        b <- peekByteOff p 1
        c <- peekByteOff p 2
        d <- peekByteOff p 3
        e <- peekByteOff p 4
        f <- peekByteOff p 5
        g <- peekByteOff p 6
        h <- peekByteOff p 7
        return $ SllAddr (a, b, c, d, e, f, g, h)

    poke p (SllAddr (a, b, c, d, e, f, g, h)) = do
        pokeByteOff p 0 a
        pokeByteOff p 1 b
        pokeByteOff p 2 c
        pokeByteOff p 3 d
        pokeByteOff p 4 e
        pokeByteOff p 5 f
        pokeByteOff p 6 g
        pokeByteOff p 7 h

type SllIfIndex = CInt
type SllHaType = CUShort
type SllHaLen = CUChar

sizeOfSockAddrLl :: SockAddrLl -> Int
sizeOfSockAddrLl SockAddrLl{}  = #const sizeof(struct sockaddr_ll)

-- | Write the given 'SockAddrLl' to the given memory location.
pokeSockAddrLl :: Ptr a -> SockAddrLl -> IO ()
pokeSockAddrLl p addr = do
    memset p 0 (#const sizeof(struct sockaddr_ll))
    (#poke struct sockaddr_ll, sll_family) p (packFamily . sllFamily $ addr)
    (#poke struct sockaddr_ll, sll_protocol) p (htons . fromIntegral . packProtocolId . sllProtocol $ addr)
    (#poke struct sockaddr_ll, sll_ifindex) p $ sllIfIndex addr
    (#poke struct sockaddr_ll, sll_hatype) p $ sllHaType addr
    (#poke struct sockaddr_ll, sll_pkttype) p (packPacketType . sllPktType $ addr)
    (#poke struct sockaddr_ll, sll_halen) p $ (addressLength . physicalAddress $ addr)
    (#poke struct sockaddr_ll, sll_addr) p $ (sllAddr . physicalAddress $ addr)

-- | Read a 'SockAddrLl' from the given memory location
peekSockAddrLl:: Ptr SockAddrLl -> IO SockAddrLl
peekSockAddrLl p = do
    sllFamily <- unpackFamily <$> (#peek struct sockaddr_ll, sll_family) p
    sllProtocol <- (unpackProtocolId . fromIntegral . ntohs) <$> (#peek struct sockaddr_ll, sll_protocol) p
    sllIfIndex <- (#peek struct sockaddr_ll, sll_ifindex) p
    sllHaType <- (#peek struct sockaddr_ll, sll_hatype) p
    sllPktType <- unpackPacketType <$> (#peek struct sockaddr_ll, sll_pkttype) p
    sllHaLen <- (#peek struct sockaddr_ll, sll_halen) p
    sllAddr <- (#peek struct sockaddr_ll, sll_addr) p
    return $ SockAddrLl sllFamily sllProtocol sllIfIndex sllHaType sllPktType (PhysicalAddress sllHaLen sllAddr)

------------------------------------------------------------------------
-- Read and Show instance for pattern-based integral newtypes

protocolIdBijection :: Bijection ProtocolId String
protocolIdBijection =
    [ (UnsupportedProtocolId, "UnsupportedProtocolId")
    , (ETH_P_LOOP, "ETH_P_LOOP")
    , (ETH_P_PUP, "ETH_P_PUP")
    , (ETH_P_PUPAT, "ETH_P_PUPAT")
    , (ETH_P_IP, "ETH_P_IP")
    , (ETH_P_X25, "ETH_P_X25")
    , (ETH_P_ARP, "ETH_P_ARP")
    , (ETH_P_BPQ, "ETH_P_BPQ")
    , (ETH_P_IEEEPUP, "ETH_P_IEEEPUP")
    , (ETH_P_IEEEPUPAT, "ETH_P_IEEEPUPAT")
    , (ETH_P_DEC, "ETH_P_DEC")
    , (ETH_P_DNA_DL, "ETH_P_DNA_DL")
    , (ETH_P_DNA_RC, "ETH_P_DNA_RC")
    , (ETH_P_DNA_RT, "ETH_P_DNA_RT")
    , (ETH_P_LAT, "ETH_P_LAT")
    , (ETH_P_DIAG, "ETH_P_DIAG")
    , (ETH_P_CUST, "ETH_P_CUST")
    , (ETH_P_SCA, "ETH_P_SCA")
    , (ETH_P_TEB, "ETH_P_TEB")
    , (ETH_P_RARP, "ETH_P_RARP")
    , (ETH_P_ATALK, "ETH_P_ATALK")
    , (ETH_P_AARP, "ETH_P_AARP")
    , (ETH_P_8021Q, "ETH_P_8021Q")
    , (ETH_P_IPX, "ETH_P_IPX")
    , (ETH_P_IPV6, "ETH_P_IPV6")
    , (ETH_P_PAUSE, "ETH_P_PAUSE")
    , (ETH_P_SLOW, "ETH_P_SLOW")
    , (ETH_P_WCCP, "ETH_P_WCCP")
    , (ETH_P_PPP_DISC, "ETH_P_PPP_DISC")
    , (ETH_P_PPP_SES, "ETH_P_PPP_SES")
    , (ETH_P_MPLS_UC, "ETH_P_MPLS_UC")
    , (ETH_P_MPLS_MC, "ETH_P_MPLS_MC")
    , (ETH_P_ATMMPOA, "ETH_P_ATMMPOA")
    , (ETH_P_ATMFATE, "ETH_P_ATMFATE")
    , (ETH_P_PAE, "ETH_P_PAE")
    , (ETH_P_AOE, "ETH_P_AOE")
    , (ETH_P_TIPC, "ETH_P_TIPC")
    , (ETH_P_1588, "ETH_P_1588")
    , (ETH_P_FCOE, "ETH_P_FCOE")
    , (ETH_P_FIP, "ETH_P_FIP")
    , (ETH_P_EDSA, "ETH_P_EDSA")
    , (ETH_P_802_3, "ETH_P_802_3")
    , (ETH_P_AX25, "ETH_P_AX25")
    , (ETH_P_ALL, "ETH_P_ALL")
    , (ETH_P_802_2, "ETH_P_802_2")
    , (ETH_P_SNAP, "ETH_P_SNAP")
    , (ETH_P_DDCMP, "ETH_P_DDCMP")
    , (ETH_P_WAN_PPP, "ETH_P_WAN_PPP")
    , (ETH_P_PPP_MP, "ETH_P_PPP_MP")
    , (ETH_P_LOCALTALK, "ETH_P_LOCALTALK")
    , (ETH_P_CAN, "ETH_P_CAN")
    , (ETH_P_PPPTALK, "ETH_P_PPPTALK")
    , (ETH_P_TR_802_2, "ETH_P_TR_802_2")
    , (ETH_P_MOBITEX, "ETH_P_MOBITEX")
    , (ETH_P_CONTROL, "ETH_P_CONTROL")
    , (ETH_P_IRDA, "ETH_P_IRDA")
    , (ETH_P_ECONET, "ETH_P_ECONET")
    , (ETH_P_HDLC, "ETH_P_HDLC")
    , (ETH_P_ARCNET, "ETH_P_ARCNET")
    , (ETH_P_DSA, "ETH_P_DSA")
    , (ETH_P_TRAILER, "ETH_P_TRAILER")
    , (ETH_P_PHONET, "ETH_P_PHONET")
    , (ETH_P_IEEE802154, "ETH_P_IEEE802154")]

instance Show ProtocolId where
    showsPrec = bijectiveShow protocolIdBijection def
      where
        gf = "GeneralProtocolId"
        def = defShow gf packProtocolId _showInt

instance Read ProtocolId where
    readPrec = bijectiveRead protocolIdBijection def
      where
        gf = "GeneralProtocolId"
        def = defRead gf unpackProtocolId _readInt

packetTypeBijection :: Bijection PacketType String
packetTypeBijection =
    [ (UnsupportedPacketType, "UnsupportedPacketType")
    , (PACKET_HOST, "PACKET_HOST")
    , (PACKET_BROADCAST, "PACKET_BROADCAST")
    , (PACKET_MULTICAST, "PACKET_MULTICAST")
    , (PACKET_OTHERHOST, "PACKET_OTHERHOST")
    , (PACKET_OUTGOING, "PACKET_OUTGOING")]

instance Show PacketType where
    showsPrec = bijectiveShow packetTypeBijection def
      where
        gf = "GeneralPacketType"
        def = defShow gf packPacketType _showInt

instance Read PacketType where
    readPrec = bijectiveRead packetTypeBijection def
      where
        gf = "GeneralPacketType"
        def = defRead gf unpackPacketType _readInt
