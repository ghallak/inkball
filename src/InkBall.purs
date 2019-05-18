module InkBall
  ( main
  ) where

import Prelude
import Effect (Effect)
import Data.Foldable (foldr, any, null, findMap)
import Data.Int (round)
import Data.List
  (List(..), fromFoldable, filter, (:))
import Data.List.NonEmpty as NE
import Data.NonEmpty (singleton, (:|))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Signal (Signal, foldp, runSignal, dropRepeats, get, sampleOn, (~>))
import Signal.Channel (Channel, channel, send, subscribe)
import Signal.Time (Time)
import Signal.DOM
  (MouseButton(..), CoordinatePair, animationFrame, mousePos,
  mouseButtonPressed)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (fromElement, offsetLeft, offsetTop)
import Web.HTML.Window (document)

import InkBall.Constants (canvasSide)
import InkBall.GameObjects
  (GameState, Sink, Ball, InkLine, GameStatus(..), Color(..), mkInkDot,
  generateBlocks, generateSinks)
import InkBall.Graphics (drawForeground, drawBackground)
import InkBall.Physics
  (moveBall, collide, fallInSink, ballCollideWithBall, ballCollideWithInkLine)

initialBall :: Ball
initialBall =
  { circle:
      { center: { x: 100.0, y: 100.0 }
      , radius: 10.0
      }
  , velocity: { x: 2.0, y: 1.0 }
  , color: Red
  }

initialBall' :: Ball
initialBall' =
  { circle:
      { center: { x: 200.0, y: 300.0 }
      , radius: 10.0
      }
  , velocity: { x: -2.0, y: 1.5 }
  , color: Green
  }

initialState :: GameState
initialState =
  { balls: initialBall : initialBall' : Nil
  , blocks: fromFoldable generateBlocks
  , sinks: fromFoldable generateSinks
  , inkLines: NE.NonEmptyList (singleton [])
  , status: Playing
  }

gameSignal :: Signal Time -> Signal (Maybe CoordinatePair) -> Signal GameState
gameSignal frames coorSig = foldp nextState initialState (sampleOn frames coorSig)

nextState :: Maybe CoordinatePair -> GameState -> GameState
nextState mCoor gameState =
  if gameState.status == Playing
    then
      let ballsNotInSinks = foldr notInSink gameState.balls gameState.sinks
          newBalls = map (moveBall <<< afterCollision) ballsNotInSinks
          lost = any (_ == true) $ fallInWrongSink <$> gameState.sinks <*> gameState.balls
          newStatus = if lost then Lost else if null newBalls then Won else Playing
          newInkLines =
            case mCoor of
              Just coor ->
                let firstLine = NE.head gameState.inkLines
                    remainingLines = NE.tail gameState.inkLines
                    newFirstLine = firstLine <> [mkInkDot coor]
                 in NE.NonEmptyList $ newFirstLine :| remainingLines
              Nothing ->
                case NE.head gameState.inkLines of
                  [] -> gameState.inkLines
                  _  -> NE.cons [] gameState.inkLines
          unhitNewInkLines = foldr notHitInkLine newInkLines gameState.balls
       in gameState { balls = newBalls, inkLines = unhitNewInkLines, status = newStatus }
    else gameState
  where
    afterCollision :: Ball -> Ball
    afterCollision =
      afterCollisionWithBlock
      <<< afterCollisionWithBall
      <<< afterCollisionWithInkLine

    afterCollisionWithBall :: Ball -> Ball
    afterCollisionWithBall ball =
      fromMaybe ball $ findMap (ballCollideWithBall ball) gameState.balls

    afterCollisionWithBlock :: Ball -> Ball
    afterCollisionWithBlock ball =
      fromMaybe ball $ findMap (collide ball) gameState.blocks

    afterCollisionWithInkLine :: Ball -> Ball
    afterCollisionWithInkLine ball =
      fromMaybe ball $ findMap (ballCollideWithInkLine ball) gameState.inkLines

    -- filter out the balls that felt into the sink
    notInSink :: Sink -> List Ball -> List Ball
    notInSink sink = filter (not <<< fallInSink sink)

    -- filter out the ink lines that were hit by a ball
    notHitInkLine :: Ball -> NE.NonEmptyList InkLine -> NE.NonEmptyList InkLine
    notHitInkLine ball inkLines =
      let unhit = NE.filter (isNothing <<< ballCollideWithInkLine ball) inkLines
       in case NE.fromList unhit of
            Just nonEmptyList -> nonEmptyList
            Nothing -> NE.NonEmptyList $ singleton []

fallInWrongSink :: Sink -> Ball -> Boolean
fallInWrongSink ball sink = fallInSink ball sink && ball.color /= sink.color

mousePosWhenClicked :: Signal Boolean -> Channel (Maybe CoordinatePair) -> CoordinatePair -> Effect Unit
mousePosWhenClicked mousePressedSignal chan coor = do
  pressed <- get mousePressedSignal
  if pressed
    then do
      offset <- canvasOffset
      let coorInCanvas = coor - offset
      if validateCoordinates coorInCanvas
        then send chan (Just coorInCanvas)
        else send chan Nothing
    else send chan Nothing
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
  elemM <- getElementById "canvas-wrapper" (toNonElementParentNode $ toDocument doc)
  case fromElement <$> elemM of
    Just (Just elem) -> do
      top <- offsetTop elem
      left <- offsetLeft elem
      pure { x: round left, y: round top }
    _ -> pure { x: 0, y: 0 }

main :: Effect Unit
main = do
  drawBackground

  mp <- mousePos
  pressed <- mouseButtonPressed MouseLeftButton
  chan <- channel Nothing
  runSignal $ (sampleOn pressed (dropRepeats mp) <> (dropRepeats mp)) ~> mousePosWhenClicked pressed chan

  frames <- animationFrame
  runSignal $ (gameSignal frames (subscribe chan)) ~> drawForeground
  pure unit
