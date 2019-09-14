{-# LANGUAGE OverloadedStrings #-}

module Game.Network
    ( createSocket
    , receiveMessage
    , Socket
    ) where

import Network.Socket hiding     (recv, recvFrom)
import Network.Socket.ByteString (recv, sendAll)

import Game.Message (Message(..))


-- FIXME: this should depend on Message
chunkSize :: Int
chunkSize = 4096


createSocket :: Int -> IO Socket
createSocket port = do
    addrinfos <- getAddrInfo Nothing (Just "localhost") (Just $ show port)
    let addr = head addrinfos
    sock <- socket (addrFamily addr) Datagram defaultProtocol
    bind sock (addrAddress addr)
    pure sock


receiveMessage :: Socket -> IO Message
receiveMessage socket = do
    bytes <- recv socket chunkSize
    print $ "raw bytes: " <> bytes

    -- FIXME: parse message for real
    pure $ NewPlayer "Player"
