{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.Linux.LinuxSpec (main, spec) where

import qualified Control.Exception as E
import Data.ByteString
import Network.Socket (Family (AF_PACKET), Socket, SocketType (Datagram, Raw), close, ifIndexToName, ifNameToIndex, socket)
import qualified Network.Socket.Address as Address
import Network.Socket.Linux
import Network.Test.Common
import Test.Hspec

testInterfaceName :: String
testInterfaceName = "haskellnetwork"

testInterfaceAddress :: PhysicalAddress
testInterfaceAddress = macAddress (0x12, 0x34, 0x56, 0x78, 0x37, 0x00)

main :: IO ()
main = do
  hspec spec

-- Look up the interface index for the given interface name or throw an exception if
-- the interface is not found
ifIndexOrFail :: IO Int
ifIndexOrFail = do
  ifIndex <- ifNameToIndex testInterfaceName
  case ifIndex of
    (Just i) -> return i
    Nothing -> error $ "Interface '" ++ testInterfaceName ++ "' not found"

-- Start at the given interface index and iterate upwards until finding an
-- index that isn't in use
nonExistingIfIndex :: Int -> IO Int
nonExistingIfIndex possibleIndex = do
  ifName <- ifIndexToName possibleIndex
  case ifName of
    Just _ -> nonExistingIfIndex (possibleIndex + 1)
    Nothing -> return possibleIndex

createSocket :: IO Socket
createSocket = socket AF_PACKET Raw (toProtocolNumber ETH_P_ALL)

prepareTest :: IO (Socket, Int, Int)
prepareTest = do
  sock <- createSocket
  ifIndex <- ifIndexOrFail
  nonExisting <- nonExistingIfIndex 123
  pure (sock, ifIndex, nonExisting)

tearDownTest :: (Socket, Int, Int) -> IO ()
tearDownTest (sock, _, _) = close sock

aroundTest :: ActionWith (Socket, Int, Int) -> IO ()
aroundTest = E.bracket prepareTest tearDownTest

spec :: Spec
spec = do
  describe "socket tests" $
    around aroundTest $ do
      describe "connect" $ do
        it "throws when attempting to connect an AF_PACKET socket" $ \(sock, ifIndex, _) -> do
          let addr = mkBindSockAddrLl AF_PACKET ETH_P_ALL ifIndex
          Address.connect sock addr `shouldThrow` anyIOException

      describe "bind" $ do
        it "binds a raw socket to an interface" $ \(sock, ifIndex, _) -> do
          let addr = mkBindSockAddrLl AF_PACKET ETH_P_ALL ifIndex
          Address.bind sock addr

        it "throws when binding to a non-existing interface" $ \(sock, _, nonExisting) -> do
          let addr = mkBindSockAddrLl AF_PACKET ETH_P_ALL nonExisting
          Address.bind sock addr `shouldThrow` anyIOException

      describe "accept" $ do
        it "throws when called on an AF_PACKET socket" $ \(sock, _, _) -> do
          (Address.accept sock :: IO (Socket, SockAddrLl)) `shouldThrow` anyIOException

      describe "sendTo" $ do
        it "throws when sending to a non-existing interface" $ \(sock, _, nonExisting) -> do
          let target = mkSendSockAddrLl AF_PACKET ETH_P_ALL nonExisting (macAddress (0x12, 0x34, 0x56, 0x78, 0x99, 0))
          Address.sendTo sock "data for you" target `shouldThrow` anyIOException

      describe "sendTo and recvFrom" $ do
        it "can send and receive data over an AF_PACKET socket" $ \(_, ifIndex, _) -> do
          let msg = "Here's some data!"
              target = mkSendSockAddrLl AF_PACKET ETH_P_ALL ifIndex (macAddress (0x12, 0x34, 0x56, 0x78, 0x99, 0))

              client sock = Address.sendTo sock msg target
              server sock = do
                (dataReceived, recipient) <- Address.recvFrom sock 1024 :: IO (ByteString, SockAddrLl)
                dataReceived `shouldBe` msg
                physicalAddress recipient `shouldBe` testInterfaceAddress

          test . setClientAction client $ packetWithClose ifIndex server

  describe "Address tests" $ do
    describe "mkMacAddress" $ do
      it "creates a PhysicalAddress with length 6" $ \_ -> do
        let addr = macAddress (1, 2, 3, 4, 5, 6)
        addressLength addr `shouldBe` 6
        address addr `shouldBe` (1, 2, 3, 4, 5, 6, 0, 0)

    describe "mkPhysicalAddress" $ do
      it "creates a PhysicalAddress" $ \_ -> do
        let addr = mkPhysicalAddress 8 (1, 2, 3, 4, 5, 6, 7, 8)
        addressLength <$> addr `shouldBe` Just 8
        address <$> addr `shouldBe` Just (1, 2, 3, 4, 5, 6, 7, 8)

      it "returns nothing when called with a negative address length" $ \_ -> do
        mkPhysicalAddress (-1) (0, 0, 0, 0, 0, 0, 0, 0) `shouldBe` Nothing

      it "returns nothing when called with an address length > 8" $ \_ -> do
        mkPhysicalAddress 9 (0, 0, 0, 0, 0, 0, 0, 0) `shouldBe` Nothing

packetWithClose ::
  Int ->
  -- | interface index
  (Socket -> IO b) ->
  -- | server action
  (ClientServer Socket b)
packetWithClose ifIndex serverAct =
   ClientServer
    { clientSetup = do
        sock <- socket AF_PACKET Datagram (toProtocolNumber ETH_P_ALL)
        return sock,
      serverSetup = do
        sock <- socket AF_PACKET Datagram (toProtocolNumber ETH_P_ALL)
        Address.bind sock (mkBindSockAddrLl AF_PACKET ETH_P_ALL ifIndex)
        return sock,
      serverAction = serverAct,
      clientAction = error "No client action defined"
    }
