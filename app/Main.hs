{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (Main.main) where

import Import
import Options.Applicative.Simple
import qualified Paths_TippyGods
import RIO.Process
import Run

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_TippyGods.version)
      "Takes one argument: the path to a CSV file containing the cards"
      "Convert a CSV file containing the cards into a text file for printing"
      ourOptions
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app run

ourOptions :: Parser Options
ourOptions = do
  optionsVerbose <- parseVerbose
  optionsInput <- parseInput
  optionsTempDir <- parseTempDir
  optionsOutput <- parseOutput
  return Options {..}

parseVerbose :: Parser Bool
parseVerbose = switch (long "verbose" <> short 'v' <> help "Verbose output?")

parseInput, parseTempDir, parseOutput :: Parser FilePath
parseInput = strArgument (metavar "FILE" <> help "A CSV file containing the cards")
parseTempDir = strOption (long "temporary-directory" <> short 'd' <> help "A temporary directory to use for working files (mostly for debugging)" <> value "" <> showDefault)
parseOutput = strOption (long "output" <> short 'o' <> help "The output file to write to" <> value "cards.svg" <> showDefault)
