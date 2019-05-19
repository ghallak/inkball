module InkBall.Signals
  ( gameSignal
  , mousePosEveryFrame
  , launchBallSignal
  ) where

import Prelude

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random (randomRange)
import Math (pi, sin, cos)
import Signal (Signal, foldp, get, dropRepeats, sampleOn, runSignal, (~>))
import Signal.Channel (Channel, channel, send, subscribe)
import Signal.DOM (CoordinatePair)
import Signal.DOM as DOM
import Signal.Time (every)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (fromElement, offsetLeft, offsetTop)
import Web.HTML.Window (document)

import InkBall.Constants (canvasSide, ballRadius)
import InkBall.GameObjects (Color(..), Ball)
import InkBall.State (SignalSum(..), GameState, nextState, initialState)

gameSignal :: Signal SignalSum -> Signal GameState
gameSignal signal = foldp nextState initialState signal

launchBallSignal :: Effect (Signal SignalSum)
launchBallSignal = do
  ballsChannel <- channel Nothing
  runSignal $ every 3000.0 ~> \_ -> randomBallsGenerator ballsChannel
  pure $ LaunchBall <$> subscribe ballsChannel

randomBallsGenerator :: Channel (Maybe Ball) -> Effect Unit
randomBallsGenerator ballsChannel = do
  let sampleBall =
        { circle:
            { center: { x: 100.0, y: 100.0 }
            , radius: ballRadius
            }
        , velocity: { x: 0.0, y: 2.0 }
        , color: Red
        }

  angle <- randomRange (-pi) pi

  let v = sampleBall.velocity
      -- rotate the velocity vector by a random angle
      -- see: https://en.wikipedia.org/wiki/Rotation_matrix
      ball = sampleBall
        { velocity
            { x = v.x * cos angle - v.y * sin angle
            , y = v.x * sin angle + v.y * cos angle
            }
        }

  send ballsChannel (Just ball)

mousePosEveryFrame :: Effect (Signal SignalSum)
mousePosEveryFrame = do
  pos <- DOM.mousePos
  pressed <- DOM.mouseButtonPressed DOM.MouseLeftButton
  posChannel <- channel Nothing
  let posNoRepeats = dropRepeats pos
      posOnPress = sampleOn pressed posNoRepeats
      posOnPressOrMove = posOnPress <> posNoRepeats
  runSignal $ posOnPressOrMove ~> mousePosWhenClicked pressed posChannel
  frame <- DOM.animationFrame
  pure $ MousePos <$> sampleOn frame (subscribe posChannel)

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
