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

setHands :: EuchreState -> [Hand] -> EuchreState
setHands st [h1, h2, h3, h4] =
  st & nthPlayer 1 . hand .~ h1
     & nthPlayer 2 . hand .~ h2
     & nthPlayer 3 . hand .~ h3
     & nthPlayer 4 . hand .~ h4

computePlayerOrder :: EuchreState -> [Int]
computePlayerOrder st = take 4 $ iterate inc (st ^. round . leaderPlayer)
