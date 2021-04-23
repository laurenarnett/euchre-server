{-# LANGUAGE BlockArguments #-}
-- |

module Euchre.RoundSpec (spec) where

import Euchre.Round
import Euchre.Utils
import Euchre.Types
import Test.Hspec
import Relude

spec :: Spec
spec = do
  describe "Part a" do
    it "1 == 1" $
      1 `shouldBe` 1

  describe "test get suit" do
    it "left is trump suit" $
      getSuit testState (Jack, Diamonds)
        `shouldBe` Hearts
    it "jack of non-trump is non-trump" $
      getSuit testState (Jack, Clubs)
        `shouldBe` Clubs

  describe "Part b" do
    it "filter valid cards" $
      filterValidCards testState [(Jack, Diamonds),
                                  (Nine, Clubs),
                                  (Ten, Clubs),
                                  (Jack, Clubs),
                                  (Queen, Clubs)]
        `shouldBe` [(Jack, Diamonds)]



testState = EuchreState {_team1 = undefined,
                         _team2 = undefined,
                         _round = Round {_roundNum = undefined,
                                         _subroundNum = undefined,
                                         _trumpSuit = Hearts,
                                         _callingTeam = undefined,
                                         _leaderPlayer = undefined,
                                         _leaderSuit = Just Hearts,
                                         _table = []}}


-- testState = EuchreState {_team1 = Team
--                                     {_player1 = Player
--                                                   {_playerId = undefined,
--                                                    _playerConn = undefined,
--                                                    _hand = [(Ace,Hearts),
--                                                             (Nine,Hearts),
--                                                             (Jack,Spades),
--                                                             (Nine,Spades),
--                                                             (Ten,Hearts)]},
--                                       _player2 = Player
--                                                    {_playerId = undefined,
--                                                     _playerConn = undefined,
--                                                     _hand = [(Jack,Hearts),
--                                                              (Nine,Clubs),
--                                                              (Jack,Clubs),
--                                                              (Ace,Spades),
--                                                              (Queen,Diamonds)]},
--                                      _points = 0},
--                           _team2 = Team
--                                      {_player1 = Player
--                                                    {_playerId = undefined,
--                                                     _playerConn = undefined,
--                                                     _hand = [(Ace,Diamonds),
--                                                              (Ten,Diamonds),
--                                                              (King,Spades),
--                                                              (Queen,Clubs),
--                                                              (Ace,Clubs)]},
--                                        _player2 = Player
--                                                     {_playerId = undefined,
--                                                      _playerConn = undefined,
--                                                      _hand = [(Nine,Diamonds),
--                                                               (King,Clubs),
--                                                               (Jack,Diamonds),
--                                                               (Queen,Hearts),
--                                                               (King,Hearts)]},
--                                        _points = 0},
--                           _round = Round
--                                      {_roundNum = 1,
--                                       _subroundNum = 0,
--                                       _trumpSuit = Hearts,
--                                       _callingTeam = 1,
--                                       _leaderPlayer = 2,
--                                       _leaderSuit = Just Hearts,
--                                       _table = [(Jack,Hearts)]}}

-- testScoreState = EuchreState {_team1 = Team
--                                          {_player1 = Player
--                                                        {_playerId = undefined,
--                                                         _playerConn = undefined,
--                                                         _hand = [(Ace,Hearts),
--                                                                  (Jack,Spades),
--                                                                  (Nine,Spades),
--                                                                  (Ten,Hearts)]},
--                                            _player2 = Player
--                                                         {_playerId = undefined,
--                                                          _playerConn = undefined,
--                                                          _hand = [(Jack,Hearts),
--                                                                   (Jack,Clubs),
--                                                                   (Ace,Spades),
--                                                                   (Queen,Diamonds)]},
--                                           _points = 0},
--                               _team2 = Team
--                                          {_player1 = Player
--                                                        {_playerId = undefined,
--                                                         _playerConn = undefined,
--                                                         _hand = [(Ace,Diamonds),
--                                                                  (Ten,Diamonds),
--                                                                  (King,Spades),
--                                                                  (Queen,Clubs)]},
--                                            _player2 = Player
--                                                         {_playerId = undefined,
--                                                          _playerConn = undefined,
--                                                          _hand = [(Nine,Diamonds),
--                                                                   (Jack,Diamonds),
--                                                                   (Queen,Hearts),
--                                                                   (King,Hearts)]},
--                                            _points = 0},
--                               _round = Round
--                                          {_roundNum = 1,
--                                           _subroundNum = 0,
--                                           _trumpSuit = Hearts,
--                                           _callingTeam = 1,
--                                           _leaderPlayer = 2,
--                                           _leaderSuit = Just Clubs,
--                                           _table = [(Nine, Clubs), (Ace, Hearts), (King, Spades), (Nine, Spades)]}}
