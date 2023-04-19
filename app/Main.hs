{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (Main.main) where

import Import
import Options.Applicative.Simple
import qualified Paths_TippyGods
import RIO.Process
import Run
import Draw

main :: IO ()
main = Draw.main
  -- do
  -- (options, ()) <-
  --   simpleOptions
  --     $(simpleVersion Paths_TippyGods.version)
  --     "Takes one argument: the path to a CSV file containing the cards"
  --     "Convert a CSV file containing the cards into a text file for printing"
  --     ( Options
  --         <$> switch
  --           ( long "verbose"
  --               <> short 'v'
  --               <> help "Verbose output?"
  --           )
  --         <*> strArgument
  --           ( metavar "FILE"
  --               <> help "A CSV file containing the cards"
  --           )
  --         <*> strOption
  --           ( long "template"
  --               <> short 't'
  --               <> help "The template file to use"
  --               <> value "template.odt"
  --               <> showDefault
  --           )
  --         <*> strOption
  --           ( long "temporary-directory"
  --               <> short 'd'
  --               <> help "A temporary directory to use for working files (mostly for debugging)"
  --               <> value ""
  --               <> showDefault
  --           )
  --         <*> strOption
  --           ( long "output"
  --               <> short 'o'
  --               <> help "The output file to write to"
  --               <> value "cards.txt"
  --               <> showDefault
  --           )
  --     )
  --     empty
  -- lo <- logOptionsHandle stderr (optionsVerbose options)
  -- pc <- mkDefaultProcessContext
  -- withLogFunc lo $ \lf ->
  --   let app =
  --         App
  --           { appLogFunc = lf,
  --             appProcessContext = pc,
  --             appOptions = options
  --           }
  --    in runRIO app run
