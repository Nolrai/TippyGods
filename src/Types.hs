{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Data.Csv hiding (Options)
import Lens
import RIO hiding (Lens')
import RIO.Process

-- | Command line arguments
data Options = Options
  { optionsVerbose :: Bool,
    optionsInput :: FilePath,
    optionsTempDir :: FilePath,
    optionsOutput :: FilePath
  }
  deriving (Show)

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options
    -- Add other app-specific configuration information here
  }

data AppData = AppData
  { appDataApp :: !App,
    appDataActionCards :: ![Card],
    appDataGods :: ![God],
    appDataLocationCards :: ![Card]
  }

data CardCore = CardCore
  { coreName :: Text,
    coreCost :: Maybe Int,
    coreText :: Text
  }
  deriving (Show)

data CardRow = CardRow
  { rowCore :: CardCore,
    rowInDeck :: Int
  }
  deriving (Show)

data Card = Card
  { cardRow :: CardRow,
    cardType :: CardType
  }
  deriving (Show)

data CardType = Location | Action
  deriving (Show)

instance FromNamedRecord CardRow where
  parseNamedRecord r = do
    (coreName, coreCost, rowInDeck', coreText) <- (,,,) <$> getName r <*> r .: "Cost" <*> r .: "# in Deck" <*> getDescription r
    let rowCore = CardCore {..}
        rowInDeck = fromMaybe 1 rowInDeck'
    pure $ CardRow rowCore rowInDeck

getName :: NamedRecord -> Parser Text
getName r = r .: "Name" <|> r .: "Location" <|> r .: "Action"

getDescription :: NamedRecord -> Parser Text
getDescription r = r .: "Description" <|> r .: ""

data God = God
  { godName :: Text,
    godLines :: [Text]
  } deriving (Show)

makeLenses ''App
makeLenses ''AppData
makeLenses ''CardCore
makeLenses ''CardRow
makeLenses ''Card

cardNameL :: Lens' Card Text
cardNameL = cardRowL . rowCoreL . coreNameL

cardCostL :: Lens' Card (Maybe Int)
cardCostL = cardRowL . rowCoreL . coreCostL

cardTextL :: Lens' Card Text
cardTextL = cardRowL . rowCoreL . coreTextL
