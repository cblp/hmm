{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)

import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Control.Monad.Trans (liftIO)

import Game.Network


newtype ServerSettings = ServerSettings { s_port :: Int } deriving Show

data ServerState = ServerState {} deriving Show

type Server = StateT ServerState IO ()


main :: IO ()
main = do
    let state = ServerState
        settings = ServerSettings 8080

    flip evalStateT state $ mainLoop settings

mainLoop :: ServerSettings -> Server
mainLoop settings = do
    liftIO $ putStrLn "GameJam server 0.0.1"

    sock <- liftIO $ createSocket $ s_port settings

    acceptPlayers sock

    liftIO $ forever $ do
        msg <- receiveMessage sock
        print msg

acceptPlayers :: Socket -> Server
acceptPlayers _socket = pure ()

