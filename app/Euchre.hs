-- |

module Euchre
  ( EuchreState (..),
    Team (..),
    Player (..)
  )
where

import Relude
import Network.Socket

data Suit = Spades | Diamonds | Hearts | Clubs deriving (Show, Eq)

data CardValue = Right | Left | Ace | King | Queen | Jack | Ten | Nine deriving (Show, Eq)

newtype Hand = Hand [(Suit, CardValue)] deriving (Show) -- newtype is a type constructor of a single element

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

data EuchreState = EuchreState
  { team1 :: Team,
    team2 :: Team,
    round :: Int
  }
  deriving (Show)
