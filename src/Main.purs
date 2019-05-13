module Main where

import Prelude
import Effect (Effect, foreachE)
import Graphics.Canvas as C
import Data.Char.Unicode (isLower, isUpper)
import Data.Foldable (foldr, any, null, findMap)
import Data.Int (round)
import Data.List
  (List(..), fromFoldable, toUnfoldable, concat, (:), head, filter, length,
  zip, (..))
import Data.List.NonEmpty as NE
import Data.NonEmpty (singleton, (:|))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Tuple (Tuple(..))
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

import GameObjects
  (GameState, Sink, Ball, Block, InkLine, GameStatus(..), Color(..), mkBlock,
  mkSink, mkInkDot)
import Graphics (drawBlock, drawBall, drawSink, drawInkLine)
import Physics
  (moveBall, collide, fallInSink, ballCollideWithBall, ballCollideWithInkLine)

canvasSide :: Number
canvasSide = 592.0

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
              Nothing -> NE.cons [] gameState.inkLines
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

drawAll :: C.Context2D -> GameState -> Effect Unit
drawAll ctx gameState = do
  C.clearRect ctx { x: 0.0, y: 0.0, width: canvasSide, height: canvasSide }
  case gameState.status of
    Playing -> do
      foreachE (NE.toUnfoldable gameState.inkLines) (drawInkLine ctx)
      foreachE (toUnfoldable gameState.balls) (drawBall ctx)
    Won -> showText "You Won" Green
    Lost -> showText "You Lost" Red
  where
    showText :: String -> Color -> Effect Unit
    showText text color = do
      C.setFont ctx "50px Comic Sans MS"
      C.setFillStyle ctx (show color)
      C.setTextAlign ctx C.AlignCenter
      C.fillText ctx text (canvasSide / 2.0) (canvasSide / 2.0)

drawBackground :: Effect Unit
drawBackground = do
  mcanvas <- C.getCanvasElementById "canvas-static"
  case mcanvas of
    Just canvas -> do
      C.setCanvasWidth canvas canvasSide
      C.setCanvasHeight canvas canvasSide
      ctx <- C.getContext2D canvas
      C.setFillStyle ctx (show DarkGray)
      C.fillRect ctx
        { x: 0.0
        , y: 0.0
        , width:  canvasSide
        , height: canvasSide
        }
      foreachE generateBlocks (drawBlock ctx)
      foreachE generateSinks (drawSink ctx)
    Nothing -> pure unit

board :: List (List Char)
board = fromFoldable <<< map fromFoldable $
  [ [ 'W','W','W','W','W','W','R','R','R','R','R','W','W','W','W','W','W' ]
  , [ 'W','.','W','.','.','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','.','.','r','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','W','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','W','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','g','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','W' ]
  , [ 'W','G','G','G','G','G','W','W','W','W','W','W','W','W','W','W','W' ]
  ]

enumBoard :: List (Tuple Char { row :: Int, col :: Int })
enumBoard =
  let rows = enumerate board
      cols = enumerate (fromMaybe Nil (head board))
      flatBoard = concat board
      enumPairs = (\r c -> { row: r, col: c }) <$> rows <*> cols
   in zip flatBoard enumPairs
  where
    enumerate :: forall a. List a -> List Int
    enumerate xs = 0..(length xs - 1)

generateBlocks :: Array Block
generateBlocks =
  let blocksCells = filter (\(Tuple c _) -> isUpper c) enumBoard
   in toUnfoldable $ map toBlock blocksCells
  where
    toBlock :: Tuple Char { row :: Int, col :: Int } -> Block
    toBlock (Tuple c coor) = mkBlock coor (charToColor c)

    charToColor :: Char -> Color
    charToColor 'G' = Green
    charToColor 'R' = Red
    charToColor 'W' = White
    charToColor 'B' = Blue
    charToColor 'Y' = Yellow
    charToColor  _  = Gray

generateSinks :: Array Sink
generateSinks =
  let sinksCells = (filter (\(Tuple c _) -> isLower c) enumBoard)
   in toUnfoldable $ map toSink sinksCells
  where
    toSink :: Tuple Char { row :: Int, col :: Int } -> Sink
    toSink (Tuple c coor) = mkSink coor (charToColor c)

    charToColor :: Char -> Color
    charToColor 'g' = Green
    charToColor 'r' = Red
    charToColor 'w' = White
    charToColor 'b' = Blue
    charToColor 'y' = Yellow
    charToColor  _  = Gray

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
  mcanvas <- C.getCanvasElementById "canvas-dynamic"
  case mcanvas of
    Just canvas -> do
      C.setCanvasWidth canvas canvasSide
      C.setCanvasHeight canvas canvasSide
      ctx <- C.getContext2D canvas

      mp <- mousePos
      pressed <- mouseButtonPressed MouseLeftButton
      chan <- channel Nothing
      runSignal $ (sampleOn pressed (dropRepeats mp) <> (dropRepeats mp)) ~> mousePosWhenClicked pressed chan

      frames <- animationFrame
      runSignal $ (gameSignal frames (subscribe chan)) ~> drawAll ctx
      pure unit
    Nothing -> pure unit
