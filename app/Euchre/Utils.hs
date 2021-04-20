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
  pure $ chunks

getNthPlayer :: EuchreState -> Int -> Player
getNthPlayer st n =
  case n of 1 -> st ^. team1 . player1
            2 -> st ^. team2 . player1
            3 -> st ^. team1 . player2
            4 -> st ^. team2 . player2

setNthPlayer :: (Eq a1, Num a1) => EuchreState -> a1 -> Lens' Player b -> b -> EuchreState
setNthPlayer st n field val =
  case n of 1 -> st & team1 . player1 . field .~ val
            2 -> st & team2 . player1 . field .~ val
            3 -> st & team1 . player2 . field .~ val
            4 -> st & team2 . player2 . field .~ val

setHands :: EuchreState -> [Hand] -> EuchreState
setHands st [h1, h2, h3, h4] =
  st
  & (\st' -> setNthPlayer st' 1 hand h1)
  & (\st' -> setNthPlayer st' 2 hand h1)
  & (\st' -> setNthPlayer st' 3 hand h3)
  & (\st' -> setNthPlayer st' 4 hand h4)

computePlayerOrder :: EuchreState -> [Int]
computePlayerOrder st = take 4 $ iterate inc (st ^. round . leaderPlayer)
