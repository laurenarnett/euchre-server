{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Relude hiding (round)

import Control.Lens
import Control.Concurrent -- forkIO
import Control.Exception
import Control.Monad (forever)
import Data.ByteString.Char8 (strip)
import Network.Socket -- assumes utf-encoded chars, so incorrectly represents binary data
import Network.Socket.ByteString -- hence, must also import Network.Socket.ByteString to correctly represent binary data
import Euchre
import System.Random.Shuffle
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
  _ <- send conn1 ("hello, you are player 1\n")
  (conn2, addr2) <- accept sock
  _ <- send conn2 ("hello, you are player 2\n")
  (conn3, addr3) <- accept sock
  _ <- send conn3 ("hello, you are player 3\n")
  (conn4, addr4) <- accept sock
  _ <- send conn4 ("hello, you are player 4\n")
  let player1 = Player addr1 conn1 []
      player2 = Player addr2 conn2 []
      player3 = Player addr3 conn3 []
      player4 = Player addr4 conn4 []
      team1 = Team player1 player3 0
      team2 = Team player2 player4 0
      round = Round 0 0 Nothing 1 2
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
  if | t1points >= 10 -> broadcast st "team 1 wins"
     | t2points >= 10 -> broadcast st "team 2 wins"
     | otherwise -> do
         st' <- playRound st
         playEuchre st'

getNthPlayer :: EuchreState -> Int -> Player
getNthPlayer st n =
  case n of 1 -> st ^. team1 . player1
            2 -> st ^. team1 . player2
            3 -> st ^. team2 . player1
            4 -> st ^. team2 . player2

setNthPlayer :: (Eq a1, Num a1) => EuchreState -> a1 -> Lens' Player b -> b -> EuchreState
setNthPlayer st n field val =
  case n of 1 -> st & team1 . player1 . field .~ val
            2 -> st & team1 . player2 . field .~ val
            3 -> st & team2 . player1 . field .~ val
            4 -> st & team2 . player2 . field .~ val

playRound :: EuchreState -> IO EuchreState
playRound st = do
  [h1, h2, h3, h4, (top:kitty)] <- dealCards
  let players = take 4 $ iterate inc (st ^. round . leaderPlayer)

  broadcast st [i|Top card: #{top}|]
  st' <- trumpSelection st top players

  pure st

inc :: Int -> Int
inc player = (player `mod` 4) + 1

trumpSelection :: EuchreState -> (CardValue, Suit) -> [Int] -> IO EuchreState
trumpSelection st top players = do
  (st', complete) <- offer st top players
  return st'

offer :: EuchreState -> (CardValue, Suit) -> [Int] -> IO (EuchreState, Bool)
offer st top [p1, p2, p3, p4] = offerCard st top p1 p2
  where
    offerCard :: EuchreState -> (CardValue, Suit) -> Int -> Int -> IO (EuchreState, Bool)
    offerCard st top dealer offeree | dealer == offeree = do
      broadcast st [i|Player #{dealer}, would you like to pick it up? [y/n]|]
      resp <- recv (getNthPlayer st offeree ^. playerConn) 256
      case strip resp of
        "n" -> pure (st, False) -- trumpSelection begins chooseYourOwnTrump
        "y" -> (, True) <$> pickUpCard st top dealer -- dealer picks up the card
    offerCard st top dealer offeree = do
      broadcast st [i|Player #{offeree}, would you like to tell player #{dealer} to pick up the card? [y/n]|]
      resp <- recv (getNthPlayer st offeree ^. playerConn) 256
      case strip resp of
        "n" -> offerCard st top dealer (inc offeree)

-- chooseYourOwnTrump :: EuchreState
-- chooseYourOwnTrump st dealer offeree top

pickUpCard :: EuchreState -> (CardValue, Suit) -> Int -> IO EuchreState
pickUpCard st top dealerPos =
  let dealer = getNthPlayer st dealerPos
      dealerConn = dealer ^. playerConn
      dealerHand = dealer ^. hand in
    do send dealerConn [i|"This is your hand: #{dealerHand}\nWhich card would you like to replace?"|]
       resp <- recv dealerConn 256
       let st' = setNthPlayer st dealerPos hand (top : dealerHand)
       -- TODO: parse card to remove and remove card from hand
       pure st'

dealCards :: IO [Hand]
dealCards = do
  cards <- shuffleM allCards
  let chunks = chunksOf 5 cards
  pure $ chunks

broadcast :: EuchreState -> ByteString -> IO ()
broadcast st msg = forM_ conns $ \conn -> send conn (msg <> "\n")
  where
    conns = [ st ^. team1 . player1 . playerConn
            , st ^. team1 . player2 . playerConn
            , st ^. team2 . player1 . playerConn
            , st ^. team2 . player2 . playerConn
            ]
