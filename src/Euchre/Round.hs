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
  send addr "Choose a card to play.\n"
  send addr [i|Valid plays: #{filterValidCards st h}|]
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
                send (st ^. nthPlayer player . playerConn)
                  [i|"Cannot play a card of a different suit if you have one of #{snd leaderCard}."|]
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
      (leaderValue, leaderSuit) = case st ^. round . leaderCard of
                              Just lCard -> lCard
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
              if | c1suit == leaderSuit && c2suit == leaderSuit -> compare c1val c2val
                 | c1suit == leaderSuit && c2suit /= leaderSuit -> GT
                 | c1suit /= leaderSuit && c2suit == leaderSuit -> LT
                 | c1suit /= leaderSuit && c2suit /= leaderSuit -> EQ

clearSubroundState :: EuchreState -> EuchreState
clearSubroundState st = st
                           & round . table .~ [] -- clear the table for the next round
                           & round . leaderCard .~ Nothing

computeTrumpOrder :: EuchreState -> [(CardValue, Suit)]
computeTrumpOrder st =
  let tSuit = st ^. round . trumpSuit
      leftSuit = case tSuit of
                   Spades -> Clubs
                   Diamonds -> Hearts
                   Hearts -> Diamonds
                   Clubs -> Spades
  in
    [(Nine, tSuit),
     (Ten, tSuit),
     (Queen, tSuit),
     (King, tSuit),
     (Ace, tSuit),
     (Jack, leftSuit),
     (Jack, tSuit)]
