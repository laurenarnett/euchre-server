{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent -- forkIO
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
  mainLoop sock chan

mainLoop :: Socket -> Chan Msg -> IO ()
mainLoop sock chan = do
  conn <- accept sock -- accept a connection and handle it
  threadId <- forkIO (runConn conn chan) -- run euchre server's logic
  mainLoop sock chan -- repeat

runConn :: (Socket, SockAddr) -> Chan Msg -> IO ()
runConn (sock, _) chan = do
  let broadcast msg = writeChan chan msg
  commLine <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  forkIO $ forever $ do
    line <- readChan commLine
    send sock line

  -- read lines from the socket and echo back to user
  forever $ do
    line <- recv sock 256
    broadcast line
