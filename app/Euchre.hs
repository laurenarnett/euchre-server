-- |

module Euchre where

import Relude
import Network.Socket

data Suit = Spades | Diamonds | Hearts | Clubs
  deriving (Show, Eq, Ord, Enum)

data CardValue = Ace | King | Queen | Jack | Ten | Nine
  deriving (Show, Eq, Ord, Enum)

allCards :: [(CardValue, Suit)]
allCards = (,) <$> [Ace .. Nine] <*> [Spades .. Clubs]

newtype Hand = Hand [(CardValue, Suit)] deriving (Show) -- newtype is a type constructor of a single element

data Player = Player
  { playerId :: SockAddr,
    playerConn :: Socket,
    hand :: Maybe Hand
  }
  deriving (Show)

data Team = Team
  { player1 :: Player,
    player2 :: Player,
    points :: Int
  }
  deriving (Show)

type PlayerId = SockAddr

data Round = Round
  { roundNum :: Int,
    subroundNum :: Int,
    trumpSuit :: Maybe Suit,
    callingTeam :: Int,
    leaderPlayer :: Int
  }
  deriving (Show)

data EuchreState = EuchreState
  { team1 :: Team,
    team2 :: Team,
    round :: Round
  }
  deriving (Show)
