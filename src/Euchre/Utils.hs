-- |
{-# LANGUAGE ApplicativeDo #-}

module Euchre.Utils where

import Relude hiding (round)
import Euchre.Types
import Control.Lens
import System.Random.Shuffle

inc :: Int -> Int
inc player = (player `mod` 4) + 1

nthPlayer :: (Num a, Eq a) => a -> Lens' (EuchreState m) (Player m)
nthPlayer n = case n of
  1 -> team1 . player1
  2 -> team2 . player1
  3 -> team1 . player2
  4 -> team2 . player2

playerToTeam :: (Num a, Eq a) => a -> Lens' (EuchreState m) (Team m)
playerToTeam n =
  case n of
    1 -> team1
    2 -> team2
    3 -> team1
    4 -> team2

players :: Traversal' (EuchreState m) (Player m)
players f st = do
  let [p1, p2, p3, p4] = computePlayerOrder st
  p1' <- f (st ^. nthPlayer p1)
  p2' <- f (st ^. nthPlayer p2)
  p3' <- f (st ^. nthPlayer p3)
  p4' <- f (st ^. nthPlayer p4)
  pure $ st & nthPlayer p1 .~ p1'
            & nthPlayer p2 .~ p2'
            & nthPlayer p3 .~ p3'
            & nthPlayer p4 .~ p4'

hands :: Lens' (EuchreState m) [Hand]
hands = partsOf' (players . hand)

getSuit :: EuchreState m -> (CardValue, Suit) -> Suit
getSuit st card =
  let tSuit = st ^. round . trumpSuit
      lSuit = getLeftSuit tSuit in
    case card of
      (Jack, lSuit') | lSuit == lSuit' -> tSuit
      (_,cardSuit) -> cardSuit

filterValidCards :: EuchreState m -> Hand -> Hand
filterValidCards st h =
  let tSuit = st ^. round . trumpSuit
      trumpCards = computeTrumpOrder st in
    case st ^. round . leaderSuit of
      Just leadSuit ->
        case filter (\card -> getSuit st card == leadSuit) h of
          [] -> h
          xs -> xs
      Nothing -> h -- player is leader: whole hand is valid

computePlayerOrder :: EuchreState m -> [Int]
computePlayerOrder st = take 4 $ iterate inc (st ^. round . leaderPlayer)

computeTrumpOrder :: EuchreState m -> [(CardValue, Suit)]
computeTrumpOrder st =
  let tSuit = st ^. round . trumpSuit
      lSuit = getLeftSuit tSuit
  in
    [(Nine, tSuit),
     (Ten, tSuit),
     (Queen, tSuit),
     (King, tSuit),
     (Ace, tSuit),
     (Jack, lSuit),
     (Jack, tSuit)]

getLeftSuit :: Suit -> Suit
getLeftSuit trumpSuit =
  case trumpSuit of
    Spades -> Clubs
    Diamonds -> Hearts
    Hearts -> Diamonds
    Clubs -> Spades
