{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
module Main where

import Relude

import Control.Concurrent -- forkIO
import Control.Exception
import Control.Monad (forever)
import Data.ByteString
import Data.ByteString.Char8 (strip)
import Network.Socket -- assumes utf-encoded chars, so incorrectly represents binary data
import Network.Socket.ByteString -- hence, must also import Network.Socket.ByteString to correctly represent binary data
import Euchre

type Msg = ByteString

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0 -- create socket
  setSocketOption sock ReuseAddr 1 -- allow socket to be reused immediately
  bind sock (SockAddrInet 4242 (tupleToHostAddress (0x7f, 0, 0, 1))) -- listen on 127.0.0.1 & TCP port 4242
  listen sock 4 -- allow socket to be reused immediately by 4 connections
  mainLoop sock
  close sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
  (conn1, addr1) <- accept sock
  _ <- send conn1 ("hello, you are player 1\n")
  (conn2, addr2) <- accept sock
  _ <- send conn2 ("hello, you are player 2\n")
  _ <- send conn2 ("what is your name?\n")
  (conn3, addr3) <- accept sock
  _ <- send conn3 ("hello, you are player 3\n")
  (conn4, addr4) <- accept sock
  _ <- send conn4 ("hello, you are player 4\n")
  resp2 <- strip <$> recv conn2 256
  -- playEuchre (Player addr1 conn1 Nothing)
  let player1 = Player addr1 conn1 Nothing
      player2 = Player addr2 conn2 Nothing
      player3 = Player addr3 conn3 Nothing
      player4 = Player addr4 conn4 Nothing
      team1 = Team player1 player3 0
      team2 = Team player2 player4 0
      euchreState = EuchreState team1 team2 0
    in playEuchre euchreState

  close conn1
  close conn2
  close conn3
  close conn4

playEuchre :: EuchreState -> IO ()
playEuchre euchreState = do
  return ()
