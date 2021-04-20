-- |

module Euchre.Round where

import Relude hiding (round)
import Control.Lens
import Control.Monad
import Euchre.Connections
import Euchre.Types
import Euchre.Utils
import Data.String.Interpolate
import Network.Socket
import Network.Socket.ByteString

playSubrounds :: EuchreState -> IO EuchreState
playSubrounds st =
  case st ^. round . roundNum of
    5 -> pure st
    _ -> do
      let st' = st & round . roundNum %~ (+ 1)
      st'' <- playTrick st'
      broadcast st'' [i|#{st''}|]
      playSubrounds st''

playTrick :: EuchreState -> IO EuchreState
playTrick st =
  let players = computePlayerOrder st in
    foldM go st players
  where
    trump = st ^. round . trumpSuit
    go st player = do
      let addr = getNthPlayer st player ^. playerConn
      send addr "Choose a card to play.\n"
      resp <- recv addr 256
      case parse (strip resp) of
        Just card -> do
          broadcast st [i|Player #{player} played #{card}.|]
          pure $ st & round . leaderPlayer .~ player
        Nothing -> pure st
