module InkBall.Signals
  ( gameSignal
  , mousePosEveryFrame
  ) where

import Prelude

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Signal (Signal, foldp, get, dropRepeats, sampleOn, runSignal, (~>))
import Signal.Channel (Channel, channel, send, subscribe)
import Signal.DOM (CoordinatePair)
import Signal.DOM as DOM
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (fromElement, offsetLeft, offsetTop)
import Web.HTML.Window (document)

import InkBall.Constants (canvasSide)
import InkBall.State (GameState, nextState, initialState)

gameSignal :: Signal (Maybe CoordinatePair) -> Signal GameState
gameSignal mousePosOnFrame = foldp nextState initialState mousePosOnFrame

mousePosEveryFrame :: Effect (Signal (Maybe CoordinatePair))
mousePosEveryFrame = do
  pos <- DOM.mousePos
  pressed <- DOM.mouseButtonPressed DOM.MouseLeftButton
  posChannel <- channel Nothing
  let posNoRepeats = dropRepeats pos
      posOnPress = sampleOn pressed posNoRepeats
      posOnPressOrMove = posOnPress <> posNoRepeats
  runSignal $ posOnPressOrMove ~> mousePosWhenClicked pressed posChannel
  frame <- DOM.animationFrame
  pure $ sampleOn frame (subscribe posChannel)

mousePosWhenClicked :: Signal Boolean
                    -> Channel (Maybe CoordinatePair)
                    -> CoordinatePair
                    -> Effect Unit
mousePosWhenClicked pressed posChannel coor = do
  isPressed <- get pressed
  if isPressed
    then do
      offset <- canvasOffset
      let coorInCanvas = coor - offset
      if validateCoordinates coorInCanvas
        then send posChannel (Just coorInCanvas)
        else send posChannel Nothing
    else send posChannel Nothing
  where
    validateCoordinates :: CoordinatePair -> Boolean
    validateCoordinates coordinatePair =
      let validX = coordinatePair.x >= 0 && coordinatePair.x <= round canvasSide
          validY = coordinatePair.y >= 0 && coordinatePair.y <= round canvasSide
       in validX && validY

canvasOffset :: Effect CoordinatePair
canvasOffset = do
  win <- window
  doc <- document win
  let parentNode = toNonElementParentNode $ toDocument doc
  elemM <- getElementById "canvas-wrapper" parentNode
  case fromElement <$> elemM of
    Just (Just elem) -> do
      top <- offsetTop elem
      left <- offsetLeft elem
      pure { x: round left, y: round top }
    _ -> pure { x: 0, y: 0 }
