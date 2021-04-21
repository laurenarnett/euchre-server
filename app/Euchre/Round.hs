-- |

module Euchre.Round where

import Relude hiding (round)
import Control.Lens
import Control.Monad
import Euchre.Connections
import Euchre.Types
import Euchre.Utils
import Data.String.Interpolate
import Network.Socket
import Network.Socket.ByteString
import qualified Data.List as L

playSubrounds :: EuchreState -> IO EuchreState
playSubrounds st =
  case st ^. round . roundNum of
    5 -> pure st
    _ -> do
      let st' = st & round . roundNum %~ (+ 1)
      st'' <- playTrick st'
      broadcast st'' [i|#{st''}|]
      -- TODO: replace leaderCard with Nothing, table with []
      playSubrounds st''

playTrick :: EuchreState -> IO EuchreState
playTrick st =
  let players = computePlayerOrder st in
    foldM playCard st players

playCard :: EuchreState -> Int -> IO EuchreState
playCard st player = do
  let addr = st ^. nthPlayer player . playerConn
  send addr "Choose a card to play.\n"
  resp <- recv addr 256
  case parse resp of
    Just card ->
        case st ^. round . leaderCard of -- if the leadercard is already set
          Just leaderCard ->
            if validatePlay st card player
              then do
                broadcast st [i|Player #{player} played #{card}.|]
                let st' = st & nthPlayer player . hand %~ L.delete card -- take from hand
                             & round . table %~ (card :) -- add to table
                pure st'
              else do
                send (st ^. nthPlayer player . playerConn) "Cannot play a card of a different suit if you have one of the leading card's suit.\n"
                playCard st player
          Nothing -> do
                broadcast st [i|Player #{player} played #{card}.|]
                let st' = st & nthPlayer player . hand %~ L.delete card -- take from hand
                          & round . table %~ (card :) -- add to table
                          & round . leaderCard ?~ card -- set leaderCard
                pure st'
    Nothing -> do
      sendInvalidInput st player
      playCard st player

validatePlay :: EuchreState -> (CardValue, Suit) -> Int -> Bool
validatePlay st (_, suit) player =
  let playerHand = st ^. nthPlayer player . hand
      -- trumpSuit = st ^. round . trumpSuit
      leaderSuit = st ^?! round . leaderCard . _Just . _2 -- get the Maybe second tuple element, or throw an exception
      handContainsSuit = leaderSuit `elem` map snd playerHand in
  not handContainsSuit || suit == leaderSuit
