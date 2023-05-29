{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module Draw (drawCards, drawGods, pageHeight, pageWidth) where

import Data.List (last)
import Data.Text as Text (pack, unpack)
import Diagrams.Prelude hiding (height, text, width)
import Graphics.SVGFonts qualified as F
import Graphics.SVGFonts.PathInRect as PIR
import Graphics.SVGFonts.Wrap qualified as FW
import Import hiding ((^.))

drawCards :: -- turn a list of card data into a grid of diagrams
  (Renderable (Path V2 Double) b) =>
  [Card] -> -- the list of cards
  QDiagram b V2 Double Any
drawCards cards =
  Import.trace (Text.pack $ __FILE__ <> " " <> show __LINE__ <> "\n" <> show cards) $ vcat (map drawRow [cards])

drawGods :: -- turn a list of god data into a grid of diagrams
  (Renderable (Path V2 Double) b) =>
  [God] -> -- the list of gods
  QDiagram b V2 Double Any
drawGods gods = vcat (map drawGodRow $ splitEvery 3 gods)

drawGodRow :: -- turn a list of god data into a row of diagrams
  (Renderable (Path V2 Double) b) =>
  [God] ->
  QDiagram b V2 Double Any
drawGodRow gods = hcat (map drawGod gods)

drawGod :: -- turn a god into a diagram
  forall b.
  (Renderable (Path V2 Double) b) =>
  God ->
  QDiagram b V2 Double Any
drawGod god@God {..} =
  Import.trace (Text.pack $ __FILE__ <> " " <> show __LINE__ <> "\n" <> show god) $
  contents # translate (V2 (-cardWidth / 2 + cornerRadius) (cardHeight / 2 - cornerRadius))
    <> roundedRect cardWidth cardHeight cornerRadius # lw 0.5 # lc black # fc white # bg white
  where
    contents :: QDiagram b V2 Double Any
    contents = vcat (name : map text godLines)
    name = textr (textWidth, nameHeight) (Text.unpack godName)
    text = textbox (textWidth, nameHeight * 2 / 3) ((cardHeight - nameHeight - 2 * cornerRadius) / fromIntegral (length godLines)) . Text.unpack

drawRow :: -- turn a list of card data into a row of diagrams
  (Renderable (Path V2 Double) b) =>
  [Card] ->
  QDiagram b V2 Double Any
drawRow cards = hcat (map drawCard cards)

exampleCard :: Card -- an example card for testing
exampleCard =
  Card
    { cardRow =
        CardRow
          { rowCore =
              CardCore
                { coreName = "Example Card",
                  coreCost = Just 1,
                  coreText = "This is an example card. It is a location card."
                },
            rowInDeck = 1
          },
      cardType = Location
    }

pageHeight, pageWidth, cardHeight, cardWidth, cornerRadius, nameHeight, textWidth, textHeight :: Fractional n => n
pageHeight = 11 -- the height of the page in tenths of an inch
pageWidth = 8.5 -- the width of the page in in tenths of an inch
cardHeight = pageHeight / 3 -- the height of a card in tenths of an inch
cardWidth = pageWidth / 3 -- the width of a card in tenths of an inch
cornerRadius = cardHeight / 10 -- the radius of the corners of the card in tenths of an inch
nameHeight = cardHeight / 10 -- the height of the name in tenths of an inch
textWidth = cardWidth - 3 * cornerRadius -- the width of the text in tenths of an inch
textHeight = cardHeight / 15 -- the height of the text in tenths of an inch

drawCard :: -- turn a card into a diagram
  (Renderable (Path V2 Double) b) =>
  Card ->
  QDiagram b V2 Double Any
drawCard card =
  Import.trace (Text.pack $ __FILE__ <> " " <> show __LINE__ <> "\n" <> show card) $
  vcat [name, text, cost] # translate (V2 (-cardWidth / 2 + cornerRadius) (cardHeight / 2 - cornerRadius))
    <> roundedRect cardWidth cardHeight cornerRadius # lw 10 # lc black # fc white # bg white
  where
    name = textr (textWidth, nameHeight) (Text.unpack $ card ^. cardNameL)
    text = textbox (textWidth, nameHeight * 2 / 3) (cardHeight - 2 * nameHeight - cornerRadius) (Text.unpack $ card ^. cardTextL)
    cost = textr (textWidth, nameHeight) costStr
    costStr = maybe "N/A" (("Cost: " <>) . show) $ card ^. cardCostL

stylize :: -- black text with blue outline on white background
  (Renderable (Path V2 Double) b) =>
  QDiagram b V2 Double Any ->
  QDiagram b V2 Double Any
stylize text = text # fc black # lc black # bg white # fillRule Winding -- # showOrigin

textDiagram :: -- turn a path in a rectangle into a diagram
  (Renderable (Path V2 Double) b) =>
  PIR.PathInRect Double ->
  QDiagram b V2 Double Any
textDiagram = stylize . F.set_envelope

-- textr fits text in a rectangle, textrs stretches spaces to fit text in a rectangle, and textp fits text in a rectangle and drops the rectangle
textr, textrs, textp :: (Renderable (Path V2 Double) b) => (Double, Double) -> String -> QDiagram b V2 Double Any
textr (width, height) text = textDiagram $ F.svgText_fitRect def (width, height) text
textrs (width, height) text = textDiagram $ F.svgText_fitRect_stretchySpace def (width, height) 5 text
textp (width, height) text = stylize $ stroke $ F.drop_rect $ F.fit_height height $ F.svgText def text

-- textbox fits text in a rectangle and splits it into lines
textbox ::
  forall b.
  (Renderable (Path V2 Double) b) =>
  (Double, Double) ->
  Double ->
  String ->
  QDiagram b V2 Double Any
textbox (width, height) boxHeight longText = 
  Import.trace (Text.pack $ __FILE__ <> " " <> show __LINE__ <> " " <> show ((width, height), boxHeight))
  stylize $ vcat $ fromLine `map` texts height
  where
    fromLine :: String -> QDiagram b V2 Double Any
    fromLine = F.set_envelope . F.svgText_fitRect_stretchySpace def (width, height) 5
    texts :: Double -> [String]
    texts height = case FW.wrapText def height splits longText of
      Just texts -> addPadLinesTo (floor (boxHeight / height)) $ Import.trace (Text.pack $ __FILE__ <> " " <> show __LINE__ <> " " <> show texts) texts
      Nothing -> if height < 0.1 then error "height too small" else texts (height - 0.1)

    splits =
      [ (FW.splitAtSpaces, (width - 30, width)),
        (FW.splitEachTwoChars, (width - 30, width)),
        (const Nothing, (-1, 1 / 0))
      ]

-- addPadLinesTo adds padding lines to the top and bottom of a list of lines
addPadLinesTo :: Int -> [String] -> [String]
addPadLinesTo numLines lines = 
  Import.trace (Text.pack $ __FILE__ <> " " <> show __LINE__ <> " " <> show result) result
  where
    result = replicate topPadding "_" ++ lines ++ replicate bottomPadding "_"
    padding = numLines - length lines
    topPadding = padding `div` 2
    bottomPadding = padding - topPadding
