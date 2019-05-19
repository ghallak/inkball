module InkBall.State
  ( GameStatus(..)
  , GameState
  , nextState
  , initialState
  ) where

import Prelude

import Data.Foldable (findMap, foldr, any, null)
import Data.List (List(..), fromFoldable, filter, (:))
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.NonEmpty (singleton, (:|))
import Signal.DOM (CoordinatePair)

import InkBall.GameObjects
  (Block, Sink, Ball, InkLine, Color(..), mkInkDot,
  generateBlocks, generateSinks)
import InkBall.Physics
  (moveBall, collide, fallInSink, ballCollideWithBall, ballCollideWithInkLine)

type GameState =
  { balls    :: List Ball
  , blocks   :: List Block
  , sinks    :: List Sink
  , inkLines :: NE.NonEmptyList InkLine
  , status   :: GameStatus
  }

data GameStatus
  = Playing
  | Won
  | Lost

derive instance eqGameStatus :: Eq GameStatus

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

    fallInWrongSink :: Sink -> Ball -> Boolean
    fallInWrongSink ball sink = fallInSink ball sink && ball.color /= sink.color

    -- filter out the balls that felt into the sink
    notInSink :: Sink -> List Ball -> List Ball
    notInSink sink = filter (not <<< fallInSink sink)

    -- filter out the ink lines that were hit by a ball
    notHitInkLine :: Ball -> NE.NonEmptyList InkLine -> NE.NonEmptyList InkLine
    notHitInkLine ball inkLines =
      let unhit = NE.filter (isNothing <<< ballCollideWithInkLine ball) inkLines
       in case NE.fromList unhit of
            Just nonEmptyList ->
              -- if the head of the list is hit, it should be replaced with []
              -- to avoid appending to the previous ink line
              if isJust $ ballCollideWithInkLine ball (NE.head inkLines)
                then NE.cons [] nonEmptyList
                else nonEmptyList
            Nothing -> NE.NonEmptyList $ singleton []
