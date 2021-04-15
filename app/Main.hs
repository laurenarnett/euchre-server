{-# LANGUAGE OverloadedStrings #-}
module Main where

import Relude

import Control.Concurrent -- forkIO
import Control.Exception
import Control.Monad (forever)
import Data.ByteString
import Network.Socket -- assumes utf-encoded chars, so incorrectly represents binary data
import Network.Socket.ByteString -- hence, must also import Network.Socket.ByteString to correctly represent binary data

type Msg = ByteString

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0 -- create socket
  setSocketOption sock ReuseAddr 1 -- allow socket to be reused immediately
  bind sock (SockAddrInet 4242 (tupleToHostAddress (0x7f, 0, 0, 1))) -- listen on 127.0.0.1 & TCP port 4242
  listen sock 2 -- set a max of 2 queued connections
  chan <- newChan
  forkIO $ forever $ do
    readChan chan
  mainLoop sock chan 0

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock -- accept a connection and handle it
  threadId <- forkIO (runConn conn chan msgNum) -- run euchre server's logic
  mainLoop sock chan (msgNum + 1) -- repeat

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, sa) chan msgNum = do
  let broadcast msg = writeChan chan msg
  commLine <- dupChan chan
  broadcast ("--> " <> show sa <> " entered chat.")

  -- fork off a thread for reading from the duplicated channel
  reader <- forkIO $ forever $ do
    line <- readChan commLine
    send sock line

  -- read lines from the socket and echo back to user
  let loop = do
          line <- recv sock 256
          case line of
            "quit\n" -> broadcast "see ya"
            "quit\r\n" -> broadcast "see ya"
            _ -> broadcast (show line) >>  loop

  loop
  -- handle (\(SomeException _) -> return ()) loop

  killThread reader
  broadcast ("--> " <> show sa <> " left chat.")
  close sock
