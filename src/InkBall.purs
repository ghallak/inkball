module Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Foldable (foldr, any, null, findMap)
import Data.Int (round)
import Data.List (List(..), fromFoldable, filter, reverse, (:))
import Data.List.NonEmpty as NE
import Data.NonEmpty (singleton, (:|))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Signal (Signal, foldp, runSignal, dropRepeats, get, sampleOn, (~>))
import Signal.Channel (Channel, channel, send, subscribe)
import Signal.Time (Time, delay)
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

gameSignal :: Signal (List (Maybe CoordinatePair)) -> Signal GameState
gameSignal coorsSignal = foldp nextState initialState coorsSignal

fff :: List (Maybe CoordinatePair) -> NE.NonEmptyList InkLine -> NE.NonEmptyList InkLine
fff Nil inkLines = inkLines
fff (Nothing : coors) inkLines =
  case NE.head inkLines of
    [] -> inkLines
    _ -> fff coors (NE.cons [] inkLines)
fff (Just coor : coors) inkLines =
  let firstLine = NE.head inkLines
      remainingLines = NE.tail inkLines
      newFirstLine = firstLine <> [mkInkDot coor]
   in fff coors (NE.NonEmptyList $ newFirstLine :| remainingLines)

nextState :: List (Maybe CoordinatePair) -> GameState -> GameState
nextState coors gameState =
  if gameState.status == Playing
    then
      let ballsNotInSinks = foldr notInSink gameState.balls gameState.sinks
          newBalls = map (moveBall <<< afterCollision) ballsNotInSinks
          lost = any (_ == true) $ fallInWrongSink <$> gameState.sinks <*> gameState.balls
          newStatus = if lost then Lost else if null newBalls then Won else Playing
          newInkLines = fff (reverse coors) gameState.inkLines
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

--mousePosWhenClicked :: Signal Boolean -> Channel (Maybe CoordinatePair) -> CoordinatePair -> Effect Unit
--mousePosWhenClicked mousePressedSignal chan coor = do
--  pressed <- get mousePressedSignal
--  if pressed
--    then do
--      offset <- canvasOffset
--      let coorInCanvas = coor - offset
--      if validateCoordinates coorInCanvas
--        then send chan (Just coorInCanvas)
--        else send chan Nothing
--    else send chan Nothing
--  where
--    validateCoordinates :: CoordinatePair -> Boolean
--    validateCoordinates coordinatePair =
--      let validX = coordinatePair.x >= 0 && coordinatePair.x <= round canvasSide
--          validY = coordinatePair.y >= 0 && coordinatePair.y <= round canvasSide
--       in validX && validY

mousePosWhenClicked :: Channel (Maybe CoordinatePair) -> MouseEvent -> Effect Unit
mousePosWhenClicked chan event =
  case event of
    Pressed _ -> do
      mp <- mousePos
      coor <- get mp
      offset <- canvasOffset
      let coorInCanvas = coor - offset
      --log $ show $ coor
      --log $ show $ offset
      --log $ show $ coorInCanvas
      --log $ show $ validateCoordinates coorInCanvas
      --log $ show $ ""
      if validateCoordinates coorInCanvas
        then send chan (Just coorInCanvas)
        else send chan Nothing
    _ -> send chan Nothing
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

data GameSignal
  = Frames Time
  | MousePos (Maybe CoordinatePair)

addMousePos :: GameSignal -> List (Maybe CoordinatePair) -> List (Maybe CoordinatePair)
addMousePos gs coors =
  case gs of
    MousePos coor -> coor : coors
    _ -> Nil

data MouseEvent
  = Pressed Boolean
  | Moved CoordinatePair

derive instance eqMouseEvent :: Eq MouseEvent

mouseCoorsEvery :: Signal Time -> Effect (Signal (List (Maybe CoordinatePair)))
mouseCoorsEvery timeSignal = do
  mp <- mousePos
  pressed <- mouseButtonPressed MouseLeftButton
  chan <- channel Nothing
  runSignal $ (((Pressed <$> pressed) <> (Moved <$> mp))) ~> mousePosWhenClicked chan
  let ts = MousePos <$> subscribe chan
  -- TODO: can the delay be shorter?
  let ff = delay 0.000001 (Frames <$> timeSignal)
  runSignal $ foldp addMousePos Nil (ts <> ff) ~> log <<< show
  pure $ sampleOn timeSignal (foldp addMousePos Nil (ts <> ff))

main :: Effect Unit
main = do
  drawBackground

  frame <- animationFrame
  mouseCoorsSignal <- mouseCoorsEvery frame
  --runSignal $ mouseCoorsSignal ~> log <<< show
  runSignal $ (gameSignal mouseCoorsSignal) ~> drawForeground
  pure unit
