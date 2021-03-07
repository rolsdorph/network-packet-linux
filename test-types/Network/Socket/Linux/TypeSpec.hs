module Network.Socket.Linux.TypeSpec (main, spec) where

import Network.Socket.Linux

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "show ProtocolId" $ do
        it "works for pattern synonyms" $
            let fam = ETH_P_ALL in
            show fam `shouldBe` "ETH_P_ALL"

        it "works for unsupported" $
            let fam = GeneralProtocolId (-1) in
            show fam `shouldBe` "UnsupportedProtocolId"

        it "works for positive values" $
            let fam = GeneralProtocolId 300 in
            show fam `shouldBe` "GeneralProtocolId 300"

        it "does not work for negative values" $
            let fam = GeneralProtocolId (-300) in
            show fam `shouldNotBe` "GeneralProtocolId -300"

    describe "show PacketType" $ do
        it "works for pattern synonyms" $
            let fam = PACKET_HOST in
            show fam `shouldBe` "PACKET_HOST"

        it "works for unsupported" $
            let fam = GeneralPacketType (-1) in
            show fam `shouldBe` "UnsupportedPacketType"

        it "works for positive values" $
            let fam = GeneralPacketType 250 in
            show fam `shouldBe` "GeneralPacketType 250"

        it "does not work for negative values" $
            let fam = GeneralPacketType (-250) in
            show fam `shouldNotBe` "GeneralPacketType -250"

    describe "bijective read-show roundtrip equality" $ do
        it "holds for ProtocolId" $ forAll protocolIdGen $
            \x -> (read . show $ x) == (x :: ProtocolId)

        it "holds for PacketType" $ forAll packetTypeGen $
            \x -> (read . show $ x) == (x :: PacketType)

-- Type-specific generators with strong bias towards pattern synonyms

-- Generator combinator that biases elements of a given list and otherwise
-- applies a function to a given generator
--
-- NOTE: The biasedGen function is copied from the network package
-- (https://github.com/haskell/network)
biasedGen :: (Gen a -> Gen b) -> [b] -> Gen a -> Gen b
biasedGen f xs g = do
    useBias <- (arbitrary :: Gen Bool)
    if useBias
       then elements xs
       else f g

protocolIdGen :: Gen ProtocolId
protocolIdGen = biasedGen (fmap GeneralProtocolId) protocolIdPatterns arbitrary

packetTypeGen :: Gen PacketType
packetTypeGen = biasedGen (fmap GeneralPacketType) packetTypePatterns arbitrary

protocolIdPatterns :: [ProtocolId]
protocolIdPatterns = [UnsupportedProtocolId
                     ,ETH_P_LOOP,ETH_P_PUP,ETH_P_PUPAT,ETH_P_IP,ETH_P_X25,ETH_P_ARP
                     ,ETH_P_BPQ,ETH_P_IEEEPUP,ETH_P_IEEEPUPAT,ETH_P_DEC,ETH_P_DNA_DL
                     ,ETH_P_DNA_RC,ETH_P_DNA_RT,ETH_P_LAT,ETH_P_DIAG,ETH_P_CUST,ETH_P_SCA
                     ,ETH_P_TEB,ETH_P_RARP,ETH_P_ATALK,ETH_P_AARP,ETH_P_8021Q,ETH_P_IPX
                     ,ETH_P_IPV6,ETH_P_PAUSE,ETH_P_SLOW,ETH_P_WCCP,ETH_P_PPP_DISC
                     ,ETH_P_PPP_SES,ETH_P_MPLS_UC,ETH_P_MPLS_MC,ETH_P_ATMMPOA
                     ,ETH_P_ATMFATE,ETH_P_PAE,ETH_P_AOE,ETH_P_TIPC,ETH_P_1588,ETH_P_FCOE
                     ,ETH_P_FIP,ETH_P_EDSA,ETH_P_802_3,ETH_P_AX25,ETH_P_ALL,ETH_P_802_2
                     ,ETH_P_SNAP,ETH_P_DDCMP,ETH_P_WAN_PPP,ETH_P_PPP_MP,ETH_P_LOCALTALK
                     ,ETH_P_CAN,ETH_P_PPPTALK,ETH_P_TR_802_2,ETH_P_MOBITEX,ETH_P_CONTROL
                     ,ETH_P_IRDA,ETH_P_ECONET,ETH_P_HDLC,ETH_P_ARCNET,ETH_P_DSA
                     ,ETH_P_TRAILER,ETH_P_PHONET,ETH_P_IEEE802154]

packetTypePatterns :: [PacketType]
packetTypePatterns = [UnsupportedPacketType,PACKET_HOST,PACKET_BROADCAST,PACKET_MULTICAST
                     ,PACKET_OTHERHOST,PACKET_OTHERHOST]
