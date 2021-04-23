-- |

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Euchre.Types where

import qualified Prelude as P
import Relude
import Relude.Base
import Control.Lens

data Suit = Spades | Diamonds | Hearts | Clubs
  deriving (Show, Eq, Ord, Enum)

-- data CardValue = Ace | King | Queen | Jack | Ten | Nine
data CardValue = Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord, Enum)

allCards :: [(CardValue, Suit)]
allCards = (,) <$> [Nine .. Ace] <*> [Spades .. Clubs]

type Hand = [(CardValue, Suit)]

data Player m = Player
  { _tell :: ByteString -> m (),
    _ask :: m ByteString,
    _playerId :: ByteString,
    _hand :: Hand
  }

instance Show (Player m) where
  show Player {..} = "Player " <> show _playerId <> "(" <> show _hand <> ")"

data Team m = Team
  { _player1 :: Player m,
    _player2 :: Player m,
    _tricksTaken :: Int,
    _points :: Int
  }
  deriving (Show)

data Round = Round
  { _roundNum :: Int,
    _subroundNum :: Int,
    _trumpSuit :: Suit,
    _callingTeam :: Int,
    _leaderPlayer :: Int,
    _leaderSuit :: Maybe Suit,
    _table :: [(CardValue, Suit)]
  }
  deriving (Show)

data EuchreState m = EuchreState
  { _team1 :: Team m,
    _team2 :: Team m,
    _round :: Round
  }
  deriving (Show)

makeLenses ''Player
makeLenses ''Round
makeLenses ''Team
makeLenses ''EuchreState
