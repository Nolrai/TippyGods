{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Data.ByteString.Lazy qualified as LByteString
import Data.Csv
import Data.Text (unlines, isSuffixOf, isPrefixOf, lines)
import qualified Data.Text as Text hiding (take)
import Diagrams.Backend.CmdLine
import Diagrams.Backend.SVG as SVG
import Diagrams.Backend.SVG.CmdLine ()
import Diagrams.Prelude (Diagram, Any, V2)
import Diagrams.Prelude qualified as D
import Draw (drawCards, drawGods, pageHeight, pageWidth)
import Import hiding (lines, unlines)
import RIO.Process
import RIO.Vector qualified as V
import System.Directory (createDirectoryIfMissing)

instance HasLogFunc AppData where
  logFuncL = appDataAppL . logFuncL

instance HasLogFunc App where
  logFuncL = appLogFuncL

instance HasProcessContext App where
  processContextL = appProcessContextL

run :: RIO App ()
run = do
  logDebug "We're inside the application!"
  Options {..} <- asks appOptions
  logInfo $ "Splitting the input file into three parts: " <> displayShow optionsInput
  (locationCards, actionCards, gods) <- splitFile optionsInput optionsTempDir readCSVSections

  let appData :: App -> AppData
      appData app =
        AppData
          { appDataApp = app,
            appDataActionCards = actionCards,
            appDataGods = gods,
            appDataLocationCards = locationCards
          }
  localRio appData writeFiles

localRio :: (r -> r') -> RIO r' a -> RIO r a
localRio f m = do
  r <- ask
  liftIO $ runRIO (f r) m

pageSizeSpec :: D.SizeSpec V2 Double
pageSizeSpec = D.mkSizeSpec2D (Just $ pageWidth * 100) (Just $ pageHeight * 100)

writeFiles :: RIO AppData ()
writeFiles = do
  AppData {..} <- ask
  let Options {..} = appOptions appDataApp
  logInfo $ "Writing the output file to " <> displayShow optionsOutput
  logInfo $ "Writing " <> displayShow (Import.length appDataActionCards) <> " cards and " <> displayShow (Import.length appDataGods) <> "gods to " <> displayShow optionsOutput
  let pages = drawPages appDataActionCards appDataGods appDataLocationCards :: [Diagram SVG]
  logInfo $ "Writing " <> displayShow (Import.length pages) <> " pages to " <> displayShow optionsOutput
  liftIO $ mapM_ (\ (n, p) -> SVG.renderSVG (show n <> "_" <> optionsOutput) pageSizeSpec p) $ zip [0 ..] pages

drawPages :: [Card] -> [God] -> [Card] -> [Diagram SVG]
drawPages l1 l2 l3 = (drawCards <$> splitEvery 9 (l1 ++ l3)) ++ (drawGods <$> splitEvery 9 l2)

splitFile :: FilePath -> FilePath -> (FilePath -> FilePath -> FilePath -> RIO App a) -> RIO App a
splitFile inputFile tempDir' next = do
  logInfo $
    if tempDir' == ""
      then "Using a temporary directory"
      else "Using the directory " <> displayShow tempDir' <> " as a temporary directory"
  let withTempDir = if tempDir' == "" then withSystemTempDirectory "" else withTempDir'
  withTempDir $ \tempDir -> do
    inputLines <- lines <$> readFileUtf8 inputFile

    let (locationLines, rest) = Import.break ("Action" `isPrefixOf`) inputLines
    let locationFile = tempDir <> "/locations.csv"
    logDebug $ "Writing location temporary file to " <> displayShow locationFile
    writeFileUtf8 locationFile (unlines locationLines)

    let (actionLines, godLines) = Import.break ("GOD #1" `isSuffixOf`) rest
    let actionFile = tempDir <> "/actions.csv"
    logDebug $ "Writing action temporary file to " <> displayShow actionFile
    writeFileUtf8 actionFile (unlines actionLines)

    let godFile = tempDir <> "/gods.csv"
    logDebug $ "Writing god temporary file to " <> displayShow godFile
    writeFileUtf8 godFile (unlines godLines)

    next actionFile godFile locationFile
  where
    withTempDir' :: (FilePath -> RIO App a) -> RIO App a
    withTempDir' f = do
      logDebug $ "Creating \'temporary\' directory " <> displayShow tempDir'
      liftIO $ createDirectoryIfMissing True tempDir'
      f tempDir'

readCSVSections :: FilePath -> FilePath -> FilePath -> RIO App ([Card], [Card], [God])
readCSVSections actionFile godFile locationFile = do
  locationCards <- decodeCards Location locationFile
  logInfo $ "Read " <> displayShow (Import.length locationCards) <> " location cards"
  actionCards <- decodeCards Action actionFile
  logInfo $ "Read " <> displayShow (Import.length actionCards) <> " action cards"
  gods <- decodeGods godFile
  logInfo $ "Read " <> displayShow (Import.length gods) <> " gods"
  pure (take 1 locationCards, take 1 actionCards, take 1 gods)

decodeCards :: CardType -> FilePath -> RIO App [Card]
decodeCards cardType file = do
  (rows :: [CardRow]) <- decodeFile file
  cards <- rows `forM` \cardRow -> pure Card {..}
  pure $ Import.filter (\ a -> notNull (a ^. cardNameL) && notNull (a ^. cardTextL) ) cards
  where
    notNull :: Text -> Bool
    notNull = not . Text.null

decodeFile :: FromNamedRecord a => FilePath -> RIO App [a]
decodeFile file = do
  logDebug $ "Reading file " <> displayShow file
  (csvData :: LByteString.ByteString) <- liftIO $ LByteString.readFile file
  logDebug $ "Parsing file " <> displayShow file
  case decodeByName csvData of
    Left err -> throwString err
    Right (_, v) -> pure $ V.toList v

decodeGods :: HasLogFunc app => FilePath -> RIO app [God]
decodeGods file = do
  logDebug $ "Reading file " <> displayShow file
  (csvData :: LByteString.ByteString) <- liftIO $ LByteString.readFile file
  logDebug $ "Parsing file " <> displayShow file
  let v :: Vector (Vector Text)
      Right v = decode NoHeader csvData
      text :: Vector Text
      Just text = V.sequence $ V.map (V.!? 3) v
  logDebug $ "god text: " <> displayShow text
  toGods text

toGods :: HasLogFunc app => Vector Text -> RIO app [God]
toGods v = 
  V.toList <$> V.mapM toGod (splitWhere Text.null v)

splitWhere :: (a -> Bool) -> Vector a -> Vector (Vector a)
splitWhere f v = 
  let (a, b) = V.break f v
  in if V.null b then V.singleton a else a `V.cons` splitWhere f (V.drop 1 b)

toGod :: HasLogFunc app => Vector Text -> RIO app God
toGod v = do
  logDebug $ "Parsing god: " <> displayShow v
  let 
    Just godName = v V.!? 0
    godLines = V.toList $ V.drop 1 v
    god = God {..}
  logDebug $ "Parsed god: " <> displayShow god
  pure god
