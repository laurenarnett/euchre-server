module Main where

import Relude hiding (round)

import Control.Lens
import Control.Lens.Regex.ByteString
import Control.Concurrent -- forkIO
import Control.Exception
import Control.Monad (forever)
import Network.Socket -- assumes utf-encoded chars, so incorrectly represents binary data
import Network.Socket.ByteString -- hence, must also import Network.Socket.ByteString to correctly represent binary data
import Euchre.Connections
import Euchre.Round
import Euchre.Types
import Euchre.Trump
import Euchre.Utils
import System.Random.Shuffle
import Data.List
import Data.List.Split
import Data.String.Interpolate


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
  _ <- send conn1 "Welcome, you are Player 1\n"
  _ <- send conn1 [i|Your address is #{addr1}\n|]
  (conn2, addr2) <- accept sock
  _ <- send conn2 "Welcome, you are Player 2\n"
  _ <- send conn2 [i|Your address is #{addr2}\n|]
  (conn3, addr3) <- accept sock
  _ <- send conn3 "Welcome, you are Player 3\n"
  _ <- send conn3 [i|Your address is #{addr3}\n|]
  (conn4, addr4) <- accept sock
  _ <- send conn4 "Welcome, you are Player 4\n"
  _ <- send conn4 [i|Your address is #{addr4}\n|]
  let player1 = Player addr1 conn1 []
      player2 = Player addr2 conn2 []
      player3 = Player addr3 conn3 []
      player4 = Player addr4 conn4 []
      team1 = Team player1 player3 0 0
      team2 = Team player2 player4 0 0
      round = Round 0 0 Hearts 1 2 Nothing [] -- dummy initial round state
      euchreState = EuchreState team1 team2 round
    in playEuchre euchreState

  close conn1
  close conn2
  close conn3
  close conn4

playEuchre :: EuchreState -> IO ()
playEuchre st = do
  let t1points = st ^. team1 . points
      t2points = st ^. team2 . points
  -- if | t1points >= 10 -> broadcast st "team 1 wins"
  --    | t2points >= 10 -> broadcast st "team 2 wins"
  --    | otherwise -> do
  st' <- playRound st
  return ()
  -- playEuchre st'

playRound :: EuchreState -> IO EuchreState
playRound st = do
  [h1, h2, h3, h4, top:kitty] <- dealTestCards -- dealCards
  let players = take 4 $ iterate inc (st ^. round . leaderPlayer)
      st' = setHands st [h1, h2, h3, h4]
  broadcast st' [i|Top card: #{top}|]
  broadcastMsgs st' (map (\player -> "Your hand:\n  " <> show (st' ^. nthPlayer player . hand)) players)
  st'' <- trumpSelection st' top players
  st''' <- playSubrounds st'
  scoreRound st'''

scoreRound :: EuchreState -> IO EuchreState
scoreRound st = do
  let (winningTeamNum, winningTeam) = if st ^. team1 . tricksTaken > st ^. team2 . tricksTaken
                                      then (1, team1)
                                      else (2, team2)
      pointsWon = if winningTeamNum == st ^. round . callingTeam
                  then 1 -- 1 point if same team as callingTeam
                  else 2 -- 2 points if callingTeam was opposing team
      st' = st & winningTeam . points %~ (+ pointsWon)
      t1Score = st' ^. team1 . points
      t2Score = st' ^. team2 . points
  broadcast st [i|Team #{winningTeamNum} won #{pointsWon} point(s).|]
  broadcast st [i|SCORE:\n  Team 1: #{t1Score}\n  Team 2: #{t2Score}|]
  pure st'

-- clearRoundState :: EuchreState -> EuchreState
