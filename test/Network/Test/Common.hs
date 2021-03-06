{-# LANGUAGE ScopedTypeVariables #-}

module Network.Test.Common where

-- The functions in this module are copied from /tests/Network/Test/Common.hs
-- in the network package (https://github.com/haskell/network)

-- | Run a client/server pair and synchronize them so that the server
-- is started before the client and the specified server action is
-- finished before the client closes the 'Socket'.

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, readMVar)
import qualified Control.Exception as E
import Network.Socket
import System.Timeout (timeout)

data ClientServer a b
    = ClientServer
    { clientSetup :: IO Socket
    , clientAction :: Socket -> IO a
    , serverSetup :: IO Socket
    , serverAction :: Socket -> IO b
    }

setClientAction
    :: (Socket -> IO b)
    -> ClientServer a c
    -> ClientServer b c
setClientAction f c = c { clientAction = f }

setServerAction
    :: (Socket -> IO c)
    -> ClientServer a b
    -> ClientServer a c
setServerAction f c = c { serverAction = f }

defaultClientServer :: ClientServer Socket Socket
defaultClientServer = ClientServer
    { clientSetup = E.throwIO $ userError "no client setup defined"
    , clientAction = return
    , serverSetup = E.throwIO $ userError "no server setup defined"
    , serverAction = return
    }

test :: ClientServer a b -> IO ()
test conf = do
    tid <- myThreadId
    barrier <- newEmptyMVar
    _ <- forkIO $ server tid barrier
    client tid barrier
  where
    server tid barrier =
        bracketWithReraise tid (serverSetup conf) close $ \sock -> do
        serverReady
        Just _ <- timeout 1000000 $ (serverAction conf) sock
        putMVar barrier ()
      where
        -- | Signal to the client that it can proceed.
        serverReady = putMVar barrier ()
    client tid barrier = do
        takeMVar barrier
        -- Transfer exceptions to the main thread.
        bracketWithReraise tid (clientSetup conf) close $ \res -> do
            Just _ <- timeout 1000000 $ (clientAction conf) res
            takeMVar barrier

-- | Like 'bracket' but catches and reraises the exception in another
-- thread, specified by the first argument.
bracketWithReraise :: ThreadId -> IO a -> (a -> IO b) -> (a -> IO ()) -> IO ()
bracketWithReraise tid setup teardown thing =
    E.bracket setup teardown thing
    `E.catch` \ (e :: E.SomeException) -> E.throwTo tid e
