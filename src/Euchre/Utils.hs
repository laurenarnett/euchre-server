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

filterValidCards :: EuchreState -> Hand -> Hand
filterValidCards st h =
  case st ^. round . leaderCard of
    Just (_,leaderSuit) ->
      case filter (\(val, suit) -> suit == leaderSuit) h of
        [] -> h
        xs -> xs
    Nothing -> h

computePlayerOrder :: EuchreState -> [Int]
computePlayerOrder st = take 4 $ iterate inc (st ^. round . leaderPlayer)

testState = EuchreState {_team1 = Team
                                    {_player1 = Player
                                                  {_playerId = undefined,
                                                   _playerConn = undefined,
                                                   _hand = [(Ace,Hearts),
                                                            (Nine,Hearts),
                                                            (Jack,Spades),
                                                            (Nine,Spades),
                                                            (Ten,Hearts)]},
                                      _player2 = Player
                                                   {_playerId = undefined,
                                                    _playerConn = undefined,
                                                    _hand = [(Jack,Hearts),
                                                             (Nine,Clubs),
                                                             (Jack,Clubs),
                                                             (Ace,Spades),
                                                             (Queen,Diamonds)]},
                                     _points = 0},
                          _team2 = Team
                                     {_player1 = Player
                                                   {_playerId = undefined,
                                                    _playerConn = undefined,
                                                    _hand = [(Ace,Diamonds),
                                                             (Ten,Diamonds),
                                                             (King,Spades),
                                                             (Queen,Clubs),
                                                             (Ace,Clubs)]},
                                       _player2 = Player
                                                    {_playerId = undefined,
                                                     _playerConn = undefined,
                                                     _hand = [(Nine,Diamonds),
                                                              (King,Clubs),
                                                              (Jack,Diamonds),
                                                              (Queen,Hearts),
                                                              (King,Hearts)]},
                                       _points = 0},
                          _round = Round
                                     {_roundNum = 1,
                                      _subroundNum = 0,
                                      _trumpSuit = Hearts,
                                      _callingTeam = 1,
                                      _leaderPlayer = 2,
                                      _leaderCard = Just (Jack,Hearts),
                                      _table = [(Jack,Hearts)]}}

testScoreState = EuchreState {_team1 = Team
                                         {_player1 = Player
                                                       {_playerId = undefined,
                                                        _playerConn = undefined,
                                                        _hand = [(Ace,Hearts),
                                                                 (Jack,Spades),
                                                                 (Nine,Spades),
                                                                 (Ten,Hearts)]},
                                           _player2 = Player
                                                        {_playerId = undefined,
                                                         _playerConn = undefined,
                                                         _hand = [(Jack,Hearts),
                                                                  (Jack,Clubs),
                                                                  (Ace,Spades),
                                                                  (Queen,Diamonds)]},
                                          _points = 0},
                              _team2 = Team
                                         {_player1 = Player
                                                       {_playerId = undefined,
                                                        _playerConn = undefined,
                                                        _hand = [(Ace,Diamonds),
                                                                 (Ten,Diamonds),
                                                                 (King,Spades),
                                                                 (Queen,Clubs)]},
                                           _player2 = Player
                                                        {_playerId = undefined,
                                                         _playerConn = undefined,
                                                         _hand = [(Nine,Diamonds),
                                                                  (Jack,Diamonds),
                                                                  (Queen,Hearts),
                                                                  (King,Hearts)]},
                                           _points = 0},
                              _round = Round
                                         {_roundNum = 1,
                                          _subroundNum = 0,
                                          _trumpSuit = Hearts,
                                          _callingTeam = 1,
                                          _leaderPlayer = 2,
                                          _leaderCard = Just (Nine, Clubs),
                                          _table = [(Nine, Clubs), (Ace, Hearts), (King, Spades), (Nine, Spades)]}}
