-- |

module Euchre.Connections where

import Relude

import Control.Lens
import Control.Lens.Regex.ByteString
import Data.String.Interpolate
import Euchre.Types
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Euchre.Utils

playerConns :: EuchreState m -> [ByteString -> m ()]
playerConns = toListOf (players . tell)

broadcast :: Monad m => EuchreState m -> ByteString -> m ()
broadcast st msg = forMOf_ (players . tell) st (\c -> c (msg <> "\n"))

broadcastMsgs :: Monad m => EuchreState m -> [ByteString] -> m ()
broadcastMsgs st = zipWithM_ (\c m -> c (m <> "\n")) (playerConns st)

sendInvalidInput :: (Monad m) => EuchreState m -> Int -> m ()
sendInvalidInput st playerNum = (st ^. nthPlayer playerNum . tell) "Invalid input. Try again.\n"

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
  case strip bs ^.. [regex|s|d|c|h|] . match of
    ["s"] -> Just Spades
    ["d"] -> Just Diamonds
    ["c"] -> Just Clubs
    ["h"] -> Just Hearts
    _ -> Nothing

parseCardValue :: ByteString -> Maybe CardValue
parseCardValue bs =
  case strip bs ^.. [regex|9|10|j|q|k|a|] . match of
    ["9"]  -> Just Nine
    ["10"] -> Just Ten
    ["j"]  -> Just Jack
    ["q"]  -> Just Queen
    ["k"]  -> Just King
    ["a"]  -> Just Ace
    _ -> Nothing

strip :: ByteString -> ByteString
strip = B.reverse . B.dropWhile isSpace . B.reverse
