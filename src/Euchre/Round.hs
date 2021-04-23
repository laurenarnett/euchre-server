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
import qualified Data.List as L hiding (elem)

playSubrounds :: EuchreState -> IO EuchreState
playSubrounds st =
  case st ^. round . subroundNum of
    5 -> pure st
    _ -> do
      let st' = st & round . subroundNum %~ (+ 1)
      st'' <- playTrick st'
      let team1Points = st'' ^. team1 . tricksTaken
          team2Points = st'' ^. team2 . tricksTaken
          newLeader = st'' ^. round . leaderPlayer
      broadcast st'' [i|TRICKS TAKEN\n  Team 1: #{team1Points}\n  Team 2: #{team2Points}|]
      playSubrounds st''

playTrick :: EuchreState -> IO EuchreState
playTrick st = do
  let leader = st ^. round . leaderPlayer
  broadcast st [i|Player #{leader} starts the next subround.|]
  -- broadcastMsgs st (map (\cards -> [i|Suggested plays: #{cards}|]) (viewHands st))
  st' <- foldM playCard st (computePlayerOrder st)
  st'' <- scoreSubround st'
  pure $ clearSubroundState st''


playCard :: EuchreState -> Int -> IO EuchreState
playCard st player = do
  let addr = st ^. nthPlayer player . playerConn
      h = st ^. nthPlayer player . hand
      validCards = filterValidCards st h
  send addr [i|Valid plays:\n  #{filterValidCards st h}\n|]
  case validCards of
    [oneCard] -> doPlayCard oneCard
    _ -> do
      send addr "Choose a card to play.\n"
      resp <- recv addr 256
      case parse resp of
        Just card -> doPlayCard card
        Nothing -> do
          sendInvalidInput st player
          playCard st player
  where
    doPlayCard card = case st ^. round . leaderSuit of -- if the leadercard is already set
          Just leaderSuit ->
            if validatePlay st card player
              then do
                broadcast st [i|Player #{player} played #{card}.|]
                let st' = st & nthPlayer player . hand %~ L.delete card -- take from hand
                             & round . table %~ (card :) -- add to table
                pure st'
              else do
                send (st ^. nthPlayer player . playerConn)
                  [i|Cannot play #{card} here.\n|]
                playCard st player
          Nothing ->
            if validatePlay st card player
            then do
              broadcast st [i|Player #{player} played #{card}.|]
              let st' = st & nthPlayer player . hand %~ L.delete card -- take from hand
                           & round . table %~ (card :) -- add to table
                           & round . leaderSuit ?~ getSuit st card -- set leaderCard
              pure st'
            else do
              send (st ^. nthPlayer player . playerConn)
                [i|Cannot play a card that is not in your hand.\n|]
              playCard st player

validatePlay :: EuchreState -> (CardValue, Suit) -> Int -> Bool
validatePlay st card player =
  let playerHand = st ^. nthPlayer player . hand in
    card `elem` filterValidCards st playerHand

scoreSubround :: EuchreState -> IO EuchreState
scoreSubround st =
    do
      let playerOrder = L.reverse $ computePlayerOrder st
          (winningCard, winningPlayerIndex) =
            L.maximumBy (\(c1, _) (c2, _) -> orderCards c1 c2) (zip (st ^. round . table) playerOrder)
      broadcast st [i|Player #{winningPlayerIndex}'s #{winningCard} won.|]
      pure $ st & playerToTeam winningPlayerIndex . tricksTaken %~ (+ 1) -- add trick to winning team
                & round . leaderPlayer .~ winningPlayerIndex -- set the leaderPlayer for the next round
    where
      leadSuit = st ^?! round . leaderSuit . _Just
      trumpOrder = computeTrumpOrder st
      orderCards :: (CardValue, Suit) -> (CardValue, Suit) -> Ordering
      orderCards (c1val, c1suit) (c2val, c2suit) =
        let c1TrumpIdx = L.elemIndex (c1val, c1suit) trumpOrder
            c2TrumpIdx = L.elemIndex (c2val, c2suit) trumpOrder in
          case (c1TrumpIdx, c2TrumpIdx) of
            (Just c1Idx, Just c2Idx) -> if c1Idx > c2Idx then GT else LT
            (Just c1Idx, Nothing) -> GT
            (Nothing, Just c2Idx) -> LT
            (Nothing, Nothing) ->
              if | c1suit == leadSuit && c2suit == leadSuit -> compare c1val c2val
                 | c1suit == leadSuit && c2suit /= leadSuit -> GT
                 | c1suit /= leadSuit && c2suit == leadSuit -> LT
                 | c1suit /= leadSuit && c2suit /= leadSuit -> EQ

clearSubroundState :: EuchreState -> EuchreState
clearSubroundState st = st
                           & round . table .~ [] -- clear the table for the next round
                           & round . leaderSuit .~ Nothing
