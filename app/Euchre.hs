-- |

module Euchre where

data Suit = Spades | Diamonds | Hearts | Clubs deriving (Show, Eq)

data CardValue = Right | Left | Ace | King | Queen | Jack | Ten | Nine deriving (Show, Eq)

data Hand = Hand [(Suit, CardValue)] deriving (Show)

data Player = Player
  { playerId :: Int,
    hand :: Hand
  }
  deriving (Show)

data Team = Team
  { player1 :: Player,
    player2 :: Player,
    points :: Int
  }
  deriving (Show)

data EuchreState = EuchreState
  { team1 :: Team,
    team2 :: Team,
    round :: Int
  }
  deriving (Show)
