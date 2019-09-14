{-# LANGUAGE OverloadedStrings #-}

module Game.Network
    ( createSocket
    , receiveMessage
    , Socket
    ) where

import Data.ByteString.Lazy.Char8 (toStrict, fromStrict)
import Data.ByteString (ByteString)

import Codec.Serialise (serialise, deserialise)

import Network.Socket hiding     (recv, recvFrom)
import Network.Socket.ByteString (recvFrom, sendAllTo)

import Game.Message (Message(..))


-- FIXME: this should depend on Message
chunkSize :: Int
chunkSize = 4096


createSocket :: Int -> IO Socket
createSocket port = do
    addrinfos <- getAddrInfo Nothing Nothing (Just $ show port)
    let addr = head addrinfos
    sock <- socket (addrFamily addr) Datagram defaultProtocol
    bind sock (addrAddress addr)
    pure sock


parseMessage :: ByteString -> Message
parseMessage = deserialise . fromStrict


unparseMessage :: Message -> ByteString
unparseMessage = toStrict . serialise


receiveMessage :: Socket -> IO (Message, SockAddr)
receiveMessage socket = do
    (bytes, addr) <- recvFrom socket chunkSize
    print $ "raw bytes: " <> bytes
    pure $ (parseMessage bytes, addr)


sendMessage :: Socket -> Message -> SockAddr -> IO ()
sendMessage socket msg = sendAllTo socket $ unparseMessage msg

