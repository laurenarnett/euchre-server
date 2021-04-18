-- |

module Euchre.Trump where

import Control.Lens
import Euchre.Types
import Euchre.Connections
import Relude hiding (round)
import Data.String.Interpolate
import Network.Socket -- assumes utf-encoded chars, so incorrectly represents binary data
import Network.Socket.ByteString -- hence, must also import Network.Socket.ByteString to correctly represent binary data
import Euchre.Utils
import Data.List

trumpSelection :: EuchreState -> (CardValue, Suit) -> [Int] -> IO EuchreState
trumpSelection st top players = do
  -- maybe (chooseYourOwnTrump st top players) pure =<< (offer st top players)
  offerRes <- offer st top players
  case offerRes of
    Just st' -> pure st'
    Nothing -> chooseYourOwnTrump st top players

offer :: EuchreState -> (CardValue, Suit) -> [Int] -> IO (Maybe EuchreState)
offer st top [p1, p2, p3, p4] = offerCard st top p4 p1
  where
    offerCard :: EuchreState -> (CardValue, Suit) -> Int -> Int -> IO (Maybe EuchreState)
    offerCard st top dealer offerer | dealer == offerer = do
      broadcast st [i|Player #{dealer}, would you like to pick it up? [y/n]|]
      resp <- recv (getNthPlayer st dealer ^. playerConn) 256
      case strip resp of
        "n" -> do
          broadcast st [i|Player #{dealer} passed.|]
          pure Nothing -- TODO: trumpSelection begins chooseYourOwnTrump
        "y" -> do
          broadcast st [i|Player #{dealer} picked up the card.|]
          Just <$> pickUpCard st top dealer
    offerCard st top dealer offerer = do
      broadcast st [i|Player #{offerer}, would you like to tell Player #{dealer} to pick up the card? [y/n]|]
      resp <- recv (getNthPlayer st offerer ^. playerConn) 8
      case strip resp of
        "n" -> do
          broadcast st [i|Player #{offerer} passed.|]
          offerCard st top dealer (inc offerer) -- ask next player
        "y" -> do
          broadcast st [i|Player #{offerer} told Player #{dealer} to pick up the card.|]
          Just <$> pickUpCard st top dealer

chooseYourOwnTrump :: EuchreState -> (CardValue, Suit) -> [Int] -> IO EuchreState
chooseYourOwnTrump st (topVal, topSuit) [p1, p2, p3, p4] = offerChoice st topSuit p4 p1
  where
    offerChoice :: EuchreState -> Suit -> Int -> Int -> IO EuchreState
    offerChoice st topSuit dealer offerer | dealer == offerer = do
      broadcast st [i|Player #{dealer}, which suit would you like? [s/c/d/h]|]
      resp <- recv (getNthPlayer st dealer ^. playerConn) 256
      case validateTrump topSuit resp of
        Just suit -> do
          broadcast st [i|Player #{dealer} chooses #{suit}|]
          pure st

        
validateTrump :: Suit -> ByteString -> Maybe Suit
validateTrump topSuit resp = undefined

  
  

pickUpCard :: EuchreState -> (CardValue, Suit) -> Int -> IO EuchreState
pickUpCard st top dealerPos = do
  let dealer = getNthPlayer st dealerPos
      dealerConn = dealer ^. playerConn
      dealerHand = dealer ^. hand
  cardToReplace <- getCardToReplace dealerConn dealerHand
  let newHand = top : delete cardToReplace dealerHand
  send dealerConn [i|This is your new hand: #{newHand}\n|]
  pure $ setNthPlayer st dealerPos hand newHand & (\st' -> st' & round . trumpSuit .~ snd top)
  where
    getCardToReplace dealerConn dealerHand = do
      send dealerConn [i|This is your hand: #{dealerHand}\nWhich card would you like to replace?\n|]
      resp <- recv dealerConn 256
      case parse (strip resp) of
        Just card -> pure card
        Nothing -> do
          send dealerConn "Failed to parse card entered. Try again.\n"
          getCardToReplace dealerConn dealerHand
