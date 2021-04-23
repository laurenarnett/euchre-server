-- |

module Euchre.Trump where

import Control.Lens
import Euchre.Types
import Euchre.Connections
import Relude hiding (round, ask)
import Data.String.Interpolate
import Euchre.Utils
import Data.List

trumpSelection :: Monad m => EuchreState m -> (CardValue, Suit) -> [Int] -> m (EuchreState m)
trumpSelection st top players = do
  -- maybe (chooseYourOwnTrump st top players) pure =<< (offer st top players)
  offerRes <- offer st top players
  case offerRes of
    Just st' -> pure st'
    Nothing -> chooseYourOwnTrump st top players

offer :: Monad m => EuchreState m -> (CardValue, Suit) -> [Int] -> m (Maybe (EuchreState m))
offer st top [p1, p2, p3, p4] = offerCard st top p4 p1

offerCard :: Monad m => EuchreState m -> (CardValue, Suit) -> Int -> Int -> m (Maybe (EuchreState m))
offerCard st top dealer offerer | dealer == offerer = do
  broadcast st [i|Player #{dealer}, would you like to pick it up? [y/n]|]
  resp <- st ^. nthPlayer dealer . ask
  case strip resp of
    "n" -> do
      broadcast st [i|Player #{dealer} passed.|]
      pure Nothing -- TODO: trumpSelection begins chooseYourOwnTrump
    "y" -> do
      broadcast st [i|Player #{dealer} picked up the card.|]
      Just <$> pickUpCard st top dealer
    _ -> do
      sendInvalidInput st dealer
      offerCard st top dealer offerer
offerCard st top dealer offerer = do
  broadcast st [i|Player #{offerer}, would you like to tell Player #{dealer} to pick up the card? [y/n]|]
  resp <- st ^. nthPlayer offerer . ask
  case strip resp of
    "n" -> do
      broadcast st [i|Player #{offerer} passed.|]
      offerCard st top dealer (inc offerer) -- ask next player
    "y" -> do
      broadcast st [i|Player #{offerer} told Player #{dealer} to pick up the card.|]
      Just <$> pickUpCard st top dealer
    _ -> do
      sendInvalidInput st offerer
      offerCard st top dealer offerer

chooseYourOwnTrump :: Monad m => EuchreState m -> (CardValue, Suit) -> [Int] -> m (EuchreState m)
chooseYourOwnTrump st (topVal, topSuit) [p1, p2, p3, p4] = offerChoice st topSuit p4 p1

offerChoice :: Monad m => EuchreState m -> Suit -> Int -> Int -> m (EuchreState m)
offerChoice st topSuit dealer offerer | dealer == offerer = do
  broadcast st [i|Player #{dealer}, which suit would you like? [s/c/d/h]|]
  resp <- st ^. nthPlayer dealer . ask
  case validateTrump topSuit resp of
    Just suit -> do
      broadcast st [i|Player #{dealer} chooses #{suit}.|]
      pure (st & round . trumpSuit .~ suit)
    Nothing -> do
      sendInvalidInput st dealer
      offerChoice st topSuit dealer offerer
offerChoice st topSuit dealer offerer = do
  broadcast st [i|Player #{offerer}, which suit would you like? [s/c/d/h/pass]|]
  resp <- st ^. nthPlayer offerer . ask
  case strip resp of
    "pass" -> do
      broadcast st [i|Player #{offerer} passed.|]
      offerChoice st topSuit dealer (inc offerer)
    _ -> do
      case validateTrump topSuit resp of
        Just suit -> do -- successfully parsed and validated a suit
          broadcast st [i|Player #{offerer} chooses #{suit}.|]
          pure (st & round . trumpSuit .~ suit)
        Nothing -> do
          sendInvalidInput st offerer
          offerChoice st topSuit dealer offerer

validateTrump :: Suit -> ByteString -> Maybe Suit
validateTrump topSuit resp =
  case parseSuit resp of
    Just suit | suit /= topSuit -> Just suit
    _ -> Nothing

pickUpCard :: (Monad m) => EuchreState m -> (CardValue, Suit) -> Int -> m (EuchreState m)
pickUpCard st top dealerPos = do
  cardToReplace <- getCardToReplace dealerHand
  let newHand = top : delete cardToReplace dealerHand
  dealerTell [i|This is your new hand: #{newHand}\n|]
  pure $ st & nthPlayer dealerPos . hand .~ newHand
            & round . trumpSuit .~ snd top
  where
    getCardToReplace dealerHand = do
      dealerTell [i|This is your hand: #{dealerHand}\nWhich card would you like to replace?\n|]
      resp <- dealerAsk
      case parse resp of
        Just card -> pure card
        Nothing -> do
          dealerTell "Failed to parse card entered. Try again.\n"
          getCardToReplace dealerHand
    dealerTell = st ^. nthPlayer dealerPos . tell
    dealerAsk = st ^. nthPlayer dealerPos . ask
    dealerHand = st ^. nthPlayer dealerPos . hand
