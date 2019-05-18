module InkBall.GameObjects
  ( Color (..)
  , BlockSide (..)
  , GameStatus (..)
  , InkDot (..)
  , Velocity
  , GameState
  , Block
  , Ball
  , Sink
  , InkLine
  , mkInkDot
  , blockSide
  , generateBlocks
  , generateSinks
  ) where

import Prelude
import Data.Char.Unicode (toLower, isLower, isUpper)
import Data.Int (toNumber)
import Data.List
  (List(..), head, concat, zip, fromFoldable, toUnfoldable, filter, length,
  (..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))

import InkBall.Geometry (Circle, Square, Vec)

data Color
  = White
  | Red
  | Blue
  | Green
  | Yellow
  | Gray
  | Black
  | DarkGray

derive instance eqColor :: Eq Color

instance showColor :: Show Color where
  show White = "White"
  show Red = "Red"
  show Blue = "Blue"
  show Green = "Green"
  show Yellow = "Yellow"
  show Gray = "Gray"
  show Black = "Black"
  show DarkGray = "DarkGray"

data BlockSide
  = TopSide
  | BottomSide
  | RightSide
  | LeftSide

data GameStatus
  = Playing
  | Won
  | Lost

type GameState =
  { balls    :: List Ball
  , blocks   :: List Block
  , sinks    :: List Sink
  , inkLines :: NonEmptyList InkLine
  , status   :: GameStatus
  }

derive instance eqGameStatus :: Eq GameStatus

type Velocity = Vec

type Block =
  { square :: Square
  , color  :: Color
  }

type Ball =
  { circle   :: Circle
  , velocity :: Velocity
  , color    :: Color
  }

type Sink =
  { square :: Square
  , circle :: Circle
  , color  :: Color
  }

newtype InkDot  = InkDot Circle

type InkLine = Array InkDot

type BoardCoordinate =
  { row :: Int
  , col :: Int
  }

mkBlock :: BoardCoordinate -> Color -> Block
mkBlock coor color =
  { square:
      { topLeft:
          { x: betweenCells + (blockSide + betweenCells) * toNumber coor.col
          , y: betweenCells + (blockSide + betweenCells) * toNumber coor.row
          }
      , side: blockSide
      }
  , color: color
  }

mkBall :: {x :: Number, y :: Number} -> Velocity -> Color -> Ball
mkBall xy vel color =
  { circle:
      { center: {x: xy.x, y: xy.y}
      , radius: ballRadius
      }
  , velocity: vel
  , color: color
  }

mkSink :: BoardCoordinate -> Color -> Sink
mkSink coor color =
  let topLeft =
        { x: (blockSide + betweenCells) * toNumber coor.col
        , y: (blockSide + betweenCells) * toNumber coor.row
        }
   in { square:
          { topLeft: topLeft
          , side: sinkSide
          }
      , circle:
          { center:
              { x: topLeft.x + sinkSide / 2.0
              , y: topLeft.y + sinkSide / 2.0
              }
          , radius: sinkHoleRadius
          }
      , color: color
      }

mkInkDot :: {x :: Int, y :: Int} -> InkDot
mkInkDot xy = InkDot
  { center: {x: toNumber xy.x, y: toNumber xy.y}
  , radius: inkRadius
  }

enumBoard :: List (Tuple Char BoardCoordinate)
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
    toBlock :: Tuple Char BoardCoordinate -> Block
    toBlock (Tuple c coor) = mkBlock coor (charToColor c)

generateSinks :: Array Sink
generateSinks =
  let sinksCells = (filter (\(Tuple c _) -> isLower c) enumBoard)
   in toUnfoldable $ map toSink sinksCells
  where
    toSink :: Tuple Char BoardCoordinate -> Sink
    toSink (Tuple c coor) = mkSink coor (charToColor c)

charToColor :: Char -> Color
charToColor c = smallCharToColor (toLower c)
  where
    smallCharToColor 'g' = Green
    smallCharToColor 'r' = Red
    smallCharToColor 'w' = White
    smallCharToColor 'b' = Blue
    smallCharToColor 'y' = Yellow
    smallCharToColor  _  = Gray

betweenCells :: Number
betweenCells = 3.0

inkRadius :: Number
inkRadius = 6.0

ballRadius :: Number
ballRadius = 16.0

sinkHoleRadius :: Number
sinkHoleRadius = 24.0

blockSide :: Number
blockSide = 32.0

sinkSide :: Number
sinkSide = 64.0

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
