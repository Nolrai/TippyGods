{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.ByteString.Lazy qualified as LByteString
import Data.Csv hiding (Options)
import RIO hiding (Lens')
import RIO.Process
import Lens

-- | Command line arguments
data Options = Options
  { optionsVerbose :: Bool,
    optionsInput :: FilePath,
    optionsTempDir :: FilePath,
    optionsTemplate :: FilePath,
    optionsOutput :: FilePath
  }

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
    appDataLocationCards :: ![Card],
    appDataTemplate :: !LByteString.ByteString
  }

data CardCore = CardCore
  { coreName :: Text,
    coreCost :: Maybe Int,
    coreText :: Text
  }

data CardRow = CardRow
  { rowCore :: CardCore,
    rowInDeck :: Int
  }

data Card = Card
  { cardRow :: CardRow,
    cardType :: CardType
  }

data CardType = Location | Action

instance FromNamedRecord CardRow where
  parseNamedRecord r = do
    (coreName, coreCost, rowInDeck, coreText) <- (,,,) <$> r .: "Name" <*> r .: "Cost" <*> r .: "# in Deck" <*> r .: "Description"
    return $ CardRow (CardCore coreName coreCost coreText) (fromMaybe 1 rowInDeck)

data God = God
  { godName :: Text,
    godLine1 :: Text,
    godLine2 :: Text,
    godLine3 :: Text
  }

-- Not actually used.
instance FromNamedRecord God where
  parseNamedRecord r =
    God <$> r .: "Name" <*> r .: "Line1" <*> r .: "Line2" <*> r .: "Line3"

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
