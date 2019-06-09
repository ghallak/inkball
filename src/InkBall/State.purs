module InkBall.State
  ( GameStatus(..)
  , SignalSum(..)
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

import InkBall.Boards (Board)
import InkBall.GameObjects
  (Block, Sink, Ball, InkLine, mkInkDot, generateBlocks, generateSinks)
import InkBall.Physics
  (moveBall, fallInSink, neighborBlocks, collideWithBlock, collideWithBall,
  collideWithInkLine)

type GameState =
  { board    :: Board
  , balls    :: List Ball
  , blocks   :: List Block
  , sinks    :: List Sink
  , inkLines :: NE.NonEmptyList InkLine
  , status   :: GameStatus
  }

data SignalSum
  = MousePos (Maybe CoordinatePair)
  | LaunchBall (Maybe Ball)

data GameStatus
  = Playing
  | Won
  | Lost

derive instance eqGameStatus :: Eq GameStatus

initialState :: Board -> GameState
initialState board =
  { board: board
  , balls: Nil
  , blocks: fromFoldable $ generateBlocks board
  , sinks: fromFoldable $ generateSinks board
  , inkLines: NE.NonEmptyList (singleton [])
  , status: Playing
  }

nextState :: SignalSum -> GameState -> GameState
nextState signal gameState =
  case signal of
    LaunchBall (Just ball) -> gameState { balls = ball : gameState.balls}
    MousePos mCoor ->
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
    _ -> gameState
  where
    afterCollision :: Ball -> Ball
    afterCollision =
      afterCollisionWithBlock
      <<< afterCollisionWithBall
      <<< afterCollisionWithInkLine

    afterCollisionWithBall :: Ball -> Ball
    afterCollisionWithBall ball =
      fromMaybe ball $ findMap (collideWithBall ball) gameState.balls

    afterCollisionWithBlock :: Ball -> Ball
    afterCollisionWithBlock ball =
      fromMaybe ball $
        findMap (collideWithBlock ball) (neighborBlocks gameState.board ball)

    afterCollisionWithInkLine :: Ball -> Ball
    afterCollisionWithInkLine ball =
      fromMaybe ball $ findMap (collideWithInkLine ball) gameState.inkLines

    fallInWrongSink :: Sink -> Ball -> Boolean
    fallInWrongSink ball sink = fallInSink ball sink && ball.color /= sink.color

    -- filter out the balls that felt into the sink
    notInSink :: Sink -> List Ball -> List Ball
    notInSink sink = filter (not <<< fallInSink sink)

    -- filter out the ink lines that were hit by a ball
    notHitInkLine :: Ball -> NE.NonEmptyList InkLine -> NE.NonEmptyList InkLine
    notHitInkLine ball inkLines =
      let unhit = NE.filter (isNothing <<< collideWithInkLine ball) inkLines
       in case NE.fromList unhit of
            Just nonEmptyList ->
              -- if the head of the list is hit, it should be replaced with []
              -- to avoid appending to the previous ink line
              if isJust $ collideWithInkLine ball (NE.head inkLines)
                then NE.cons [] nonEmptyList
                else nonEmptyList
            Nothing -> NE.NonEmptyList $ singleton []
