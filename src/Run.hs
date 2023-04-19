{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Run (run) where

import Data.ByteString.Lazy qualified as LByteString
import Data.Csv
import Data.Text as Text
import Import hiding (lines, unlines)
import System.Directory (createDirectoryIfMissing)
import RIO.Vector qualified as V
import RIO.Process

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
  logInfo
    ( "Splitting the input file into three parts: "
        <> displayShow optionsInput
    )
  logInfo $ if optionsTempDir == ""
    then "Using a temporary directory" 
    else "Using the directory \"" <> displayShow optionsTempDir <> "\" as a temporary directory"
  (locationCards, actionCards, gods) <- splitFile optionsInput optionsTempDir readCSVSections

  logDebug "Reading the template"
  template <- liftIO $ LByteString.readFile optionsTemplate

  let
    appData :: App -> AppData 
    appData app =
        AppData
          { appDataApp = app,
            appDataActionCards = actionCards,
            appDataGods = gods,
            appDataLocationCards = locationCards,
            appDataTemplate = template
          }
  localRio appData writeFiles

localRio :: (r -> r') -> RIO r' a -> RIO r a
localRio f m = do
  r <- ask
  liftIO $ runRIO (f r) m

writeFiles :: RIO AppData ()
writeFiles = do
  AppData {..} <- ask
  Just v <- pure Nothing
  pure v

splitFile :: FilePath -> FilePath -> (FilePath -> FilePath -> FilePath -> RIO App a) -> RIO App a
splitFile inputFile tempDir' next = do
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
  actionCards <- decodeCards Action actionFile
  gods <- decodeGods godFile
  locationCards <- decodeCards Location locationFile
  pure (locationCards, actionCards, gods)

decodeCards :: CardType -> FilePath -> RIO App [Card]
decodeCards cardType file = do
  (rows :: [CardRow]) <- decodeFile file
  rows `forM` \ cardRow -> pure Card {..} 

decodeFile :: FromNamedRecord a => FilePath -> RIO App [a]
decodeFile file = do
  logDebug $ "Reading file " <> displayShow file
  (csvData :: LByteString.ByteString) <- liftIO $ LByteString.readFile file
  logDebug $ "Parsing file " <> displayShow file
  case decodeByName csvData of
    Left err -> throwString err
    Right (_, v) -> pure $ V.toList v

decodeGods :: FilePath -> RIO App [God]
decodeGods file = do
  logDebug $ "Reading file " <> displayShow file
  (csvData :: LByteString.ByteString) <- liftIO $ LByteString.readFile file
  logDebug $ "Parsing file " <> displayShow file
  let
    v :: Vector (Vector Text) 
    Right v = decode NoHeader csvData
    text :: Vector Text
    Just text = V.sequence $ V.map (V.!? 3) v
  toGods (V.filter Text.null text)

toGods :: MonadFail m => Vector Text -> m [God]
toGods v
  | V.null v = pure []
  | otherwise = do
    let
      front = V.take 4 v
      rest = V.drop 4 v
      [godName, godLine1, godLine2, godLine3] = V.toList front
    (God {..} :) <$> toGods rest

instance MonadFail (RIO app) where
  fail = throwString
