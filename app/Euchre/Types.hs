{-# LANGUAGE TemplateHaskell #-}
-- |

module Euchre.Types where

import Relude
import Network.Socket
import Control.Lens

data Suit = Spades | Diamonds | Hearts | Clubs
  deriving (Show, Eq, Ord, Enum)

data CardValue = Ace | King | Queen | Jack | Ten | Nine
  deriving (Show, Eq, Ord, Enum)

allCards :: [(CardValue, Suit)]
allCards = (,) <$> [Ace .. Nine] <*> [Spades .. Clubs]

type Hand = [(CardValue, Suit)]

data Player = Player
  { _playerId :: SockAddr,
    _playerConn :: Socket,
    _hand :: Hand
  }
  deriving (Show)

data Team = Team
  { _player1 :: Player,
    _player2 :: Player,
    _points :: Int
  }
  deriving (Show)

type PlayerId = SockAddr

data Round = Round
  { _roundNum :: Int,
    _subroundNum :: Int,
    _trumpSuit :: Maybe Suit,
    _callingTeam :: Int,
    _leaderPlayer :: Int
  }
  deriving (Show)

data EuchreState = EuchreState
  { _team1 :: Team,
    _team2 :: Team,
    _round :: Round
  }
  deriving (Show)

makeLenses ''Player
makeLenses ''Round
makeLenses ''Team
makeLenses ''EuchreState
