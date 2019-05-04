module Main where

import Prelude
import Effect (Effect, foreachE)
import Graphics.Canvas as C
import Data.List
  (List(..), fromFoldable, toUnfoldable, concat, (:), head, catMaybes)
import Data.Maybe (Maybe(..), fromMaybe)
import Signal (Signal, foldp, runSignal, (~>))
import Signal.Time (Time)
import Signal.DOM (animationFrame)

import GameObjects (Sink, Ball, Block, Color(..), mkBlock, mkSink)
import Graphics (drawBlock, drawBall, drawSink)
import Physics (moveBall, collide)

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

ballSignal :: Array Block -> Signal Time -> Signal Ball
ballSignal blocks frames = foldp (\_ -> moveBall <<< f) initialBall frames
  where
    f :: Ball -> Ball
    f ball = fromMaybe ball <<< head <<< catMaybes <<< fromFoldable $ map (collide ball) blocks

drawAll :: C.Context2D -> Ball -> Effect Unit
drawAll ctx ball = do
  C.clearRect ctx { x: 0.0, y: 0.0, width: canvasSide, height: canvasSide }
  drawBall ctx ball

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
  [ [ 'W','W','W','W','W','W','W','W','W','W','W','W','W','W','W','W','W' ]
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
  , [ 'W','W','W','W','W','W','W','W','W','W','W','W','W','W','W','W','W' ]
  ]

generateBlocks :: Array Block
generateBlocks = toUnfoldable <<< concat $ genRows 0 board
  where
    genRows :: Int -> List (List Char) -> List (List Block)
    genRows r (row : rows) = genCols r 0 row : genRows (r + 1) rows
    genRows _ Nil          = Nil

    genCols :: Int -> Int -> List Char -> List Block
    genCols rowNum colNum ('W' : cols) = mkBlock { row: rowNum, col: colNum } Gray : genCols rowNum (colNum + 1) cols
    genCols rowNum colNum ( _  : cols) = genCols rowNum (colNum + 1) cols
    genCols _      _      Nil          = Nil

generateSinks :: Array Sink
generateSinks = toUnfoldable <<< concat $ genRows 0 board
  where
    genRows :: Int -> List (List Char) -> List (List Sink)
    genRows r (row : rows) = genCols r 0 row : genRows (r + 1) rows
    genRows _ Nil          = Nil

    genCols :: Int -> Int -> List Char -> List Sink
    genCols rowNum colNum ('g' : cols) = mkSink { row: rowNum, col: colNum } Green : genCols rowNum (colNum + 1) cols
    genCols rowNum colNum ('r' : cols) = mkSink { row: rowNum, col: colNum } Red : genCols rowNum (colNum + 1) cols
    genCols rowNum colNum ( _  : cols) = genCols rowNum (colNum + 1) cols
    genCols _      _      Nil          = Nil

main :: Effect Unit
main = do
  drawBackground
  mcanvas <- C.getCanvasElementById "canvas-dynamic"
  case mcanvas of
    Just canvas -> do
      C.setCanvasWidth canvas canvasSide
      C.setCanvasHeight canvas canvasSide
      ctx <- C.getContext2D canvas
      let blocks = generateBlocks
      frames <- animationFrame
      runSignal $ (ballSignal blocks frames) ~> drawAll ctx
      pure unit
    Nothing -> pure unit
