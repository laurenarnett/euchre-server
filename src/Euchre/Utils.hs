-- |

module Euchre.Utils where

import Relude hiding (round)
import Euchre.Types
import Control.Lens
import System.Random.Shuffle
import Data.List.Split

inc :: Int -> Int
inc player = (player `mod` 4) + 1


dealCards :: IO [Hand]
dealCards = do
  cards <- shuffleM allCards
  let chunks = chunksOf 5 cards
  pure chunks

dealTestCards :: IO [Hand]
dealTestCards = do
  _ <- shuffleM allCards
  pure [[(Ace,Hearts),(Nine,Hearts),(Jack,Clubs),(Queen,Diamonds),(King,Diamonds)],
   [(Ace,Diamonds),(Queen,Hearts),(Jack,Hearts),(Ace,Spades),(Nine,Spades)],
   [(Jack,Diamonds),(Nine,Diamonds),(Jack,Spades),(Ten,Hearts),(Nine,Clubs)],
   [(Ten,Spades),(King,Spades),(King,Hearts),(Ace,Clubs),(Queen,Spades)],
   [(Ten,Clubs),(King,Clubs),(Queen,Clubs),(Ten,Diamonds)]]

nthPlayer :: (Num a, Eq a) => a -> Lens' EuchreState Player
nthPlayer n = case n of
  1 -> team1 . player1
  2 -> team2 . player1
  3 -> team1 . player2
  4 -> team2 . player2

playerToTeam :: (Num a, Eq a) => a -> Lens' EuchreState Team
playerToTeam n =
  case n of
    1 -> team1
    2 -> team2
    3 -> team1
    4 -> team2

setHands :: EuchreState -> [Hand] -> EuchreState
setHands st [h1, h2, h3, h4] =
  let [p1, p2, p3, p4] = computePlayerOrder st in
    st & nthPlayer p1 . hand .~ h1
       & nthPlayer p2 . hand .~ h2
       & nthPlayer p3 . hand .~ h3
       & nthPlayer p4 . hand .~ h4

viewHands :: EuchreState -> [Hand]
viewHands st =
  let players = computePlayerOrder st in
    map (\player -> filterValidCards st $ st ^. nthPlayer player . hand) players

getSuit :: EuchreState -> (CardValue, Suit) -> Suit
getSuit st card =
  let tSuit = st ^. round . trumpSuit
      lSuit = getLeftSuit tSuit in
    case card of
      (Jack, lSuit') | lSuit == lSuit' -> tSuit
      (_,cardSuit) -> cardSuit

filterValidCards :: EuchreState -> Hand -> Hand
filterValidCards st h =
  let tSuit = st ^. round . trumpSuit
      trumpCards = computeTrumpOrder st in
    case st ^. round . leaderSuit of
      Just leadSuit ->
        case filter (\card -> getSuit st card == leadSuit) h of
          [] -> h
          xs -> xs
      Nothing -> h -- player is leader: whole hand is valid

computePlayerOrder :: EuchreState -> [Int]
computePlayerOrder st = take 4 $ iterate inc (st ^. round . leaderPlayer)

computeTrumpOrder :: EuchreState -> [(CardValue, Suit)]
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
