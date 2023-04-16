{-# LANGUAGE NoImplicitPrelude #-}

module Card where

import Data.Csv
import Relude
import Rio

data CardCore = CardRow
  { cardName :: Text
  , cardCost :: Int
  , cardText :: Text
  }

data CardRow = CardRow
  { rowCore :: CardCore
  , rowInDeck :: Int
  }

data Card = Card
  { cardRow :: CardRow
  , cardType :: CardType
  }

data CardType = Location | Action

instance FromNamedRecord CardRow where
  parseNamedRecord r = do
    (cardName, cardCost, rowInDeck, cardText) <- (,,,) <$> r .: "Name" <*> r .: "Cost" <*> r .: "# in Deck" <*> r .: "Description"
    return $ CardRow (CardCore cardName cardCost cardText) rowInDeck

data God = God
  { godName :: Text
  , godLine1 :: Text
  , godLine2 :: Text
  , godLine3 :: Text
  }

instance FromNamedRecord God where
  parseNamedRecord r =
    God <$> r .: "Name" <*> r .: "Line1" <*> r .: "Line2" <*> r .: "Line3"
