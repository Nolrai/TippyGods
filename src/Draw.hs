{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Draw (main) where

import Import hiding ((^.))
import Diagrams.Prelude hiding (text, width, height)
import Diagrams.TwoD as Diagrams hiding (text, width, height)
import Diagrams.Backend.SVG as SVG
import Diagrams.Backend.SVG.CmdLine as SVG
import Diagrams.Backend.Postscript as Postscript
import Data.List (init, last)
import qualified Graphics.SVGFonts as F
import qualified Graphics.SVGFonts.Wrap as FW

-- pageHight :: Num n => n
-- pageHight = 11

-- pageWidth :: Num n => n
-- pageWidth = 8.5

-- cardHight = pageHight / 3
-- cardWidth = pageWidth / 3

-- cornerRadius = cardHight / 10

-- fontSizeValue = cardHight / 20

-- drawCard :: Card -> Diagram Postscript.B
-- drawCard card =
--   roundedRect cardHight cardWidth cornerRadius # lw 0.5 # fc black
--     <> topLeftText (card ^. cardNameL) # fontSize fontSizeValue # fc black # translate (r2 (- (cardWidth - cornerRadius) , - cardHight / 2 ))
--     <> text (card ^. cardTextL) # fontSize fontSizeValue # fc black # centerXY
--     <> maybe mempty (text . ("Cost: " <>) . displayShow ) (card ^. cardCostL) # fontSize fontSizeValue # fc black # translate (r2 (0, cardHight / 2))

main = mainWith diagram

diagram :: Diagram SVG.B
diagram = vsep 1 [textw, texth, textr, textrs, textp, textbox]

text = "Hello World, ahoy!"
longText =
  "At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis \
  \praesentium voluptatum deleniti atque corrupti quos dolores."

width = 200 :: Double
height = 22

stylize text = text # fc blue # lc blue # bg lightgrey # fillRule EvenOdd # showOrigin
textDiagram = stylize . F.set_envelope

textw = textDiagram$ F.fit_width width$ F.svgText def text
texth = textDiagram$ F.fit_height height$ F.svgText def text
textr = textDiagram$ F.svgText_fitRect def (width, height) text
textrs = textDiagram$ F.svgText_fitRect_stretchySpace def (width, height) 5 text
textp = stylize$ stroke$ F.drop_rect$ F.fit_height height$ F.svgText def text

textbox = stylize$ vcat$ map F.set_envelope$
  map (F.svgText_fitRect_stretchySpace def (width, height) 5) (init texts)
  ++ [F.fit_height height$ F.svgText def$ last texts]
  where
    texts = case FW.wrapText def height splits longText of
      Just texts -> texts
      Nothing -> map return longText

    splits =
      [ (FW.splitAtSpaces, (width - 30, width + 10))
      , (FW.splitEachTwoChars, (width - 30, width + 10))
      , (const Nothing, (-1, 1/0))
      ]
