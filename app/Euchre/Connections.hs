-- |

module Euchre.Connections where

import Relude

import Control.Lens
import Control.Lens.Regex.ByteString
import Data.String.Interpolate
import Euchre.Types
import Network.Socket -- assumes utf-encoded chars, so incorrectly represents binary data
import Network.Socket.ByteString -- hence, must also import Network.Socket.ByteString to correctly represent binary data
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Euchre.Utils

playerConns :: EuchreState -> [Socket]
playerConns st = [ st ^. team1 . player1 . playerConn
                 , st ^. team1 . player2 . playerConn
                 , st ^. team2 . player1 . playerConn
                 , st ^. team2 . player2 . playerConn
                 ]

broadcast :: EuchreState -> ByteString -> IO ()
broadcast st msg = forM_ (playerConns st) $ \conn -> send conn (msg <> "\n")

broadcastMsgs :: EuchreState -> [ByteString] -> IO ()
broadcastMsgs st [m1, m2, m3, m4] = do
  let [c1, c2, c3, c4] = playerConns st
  void $ send c1 (m1 <> "\n")
  void $ send c2 (m2 <> "\n")
  void $ send c3 (m3 <> "\n")
  void $ send c4 (m4 <> "\n")

sendInvalidInput :: EuchreState -> Int -> IO ()
sendInvalidInput st playerNum = void $ send (st ^. nthPlayer playerNum . playerConn) "Invalid input. Try again."

parse :: ByteString -> Maybe (CardValue, Suit)
parse bs =
  case parseSuit bs of
    Just s ->
      case parseCardValue bs of
        Just v -> Just (v, s)
        _ -> Nothing
    _ -> Nothing

parseSuit :: ByteString -> Maybe Suit
parseSuit bs =
  case bs ^.. [regex|s|d|c|h|] . match of
    ["s"] -> Just Spades
    ["d"] -> Just Diamonds
    ["c"] -> Just Clubs
    ["h"] -> Just Hearts
    _ -> Nothing

parseCardValue :: ByteString -> Maybe CardValue
parseCardValue bs =
  case bs ^.. [regex|9|10|j|q|k|a|] . match of
    ["9"]  -> Just Nine
    ["10"] -> Just Ten
    ["j"]  -> Just Jack
    ["q"]  -> Just Queen
    ["k"]  -> Just King
    ["a"]  -> Just Ace
    _ -> Nothing

strip :: ByteString -> ByteString
strip = B.reverse . B.dropWhile isSpace . B.reverse
