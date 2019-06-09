module InkBall.GameObjects
  ( Color (..)
  , BlockSide (..)
  , InkDot (..)
  , Block
  , Ball
  , Sink
  , InkLine
  , BallSource
  , BoardCoordinate
  , mkInkDot
  , generateBlocks
  , generateBlocksMap
  , generateSinks
  , generateBallSources
  ) where

import Prelude

import Data.Char.Unicode (toLower, isLower, isUpper)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Int (toNumber)
import Data.List
  (List(..), head, concat, zip, toUnfoldable, filter, length, (..))
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))

import InkBall.Boards (board)
import InkBall.Constants
  (betweenCells, ballRadius, blockSide, inkRadius, sinkSide, sinkHoleRadius,
  ballSourceRadius)
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
  show White    = "White"
  show Red      = "Red"
  show Blue     = "Blue"
  show Green    = "Green"
  show Yellow   = "Yellow"
  show Gray     = "Gray"
  show Black    = "Black"
  show DarkGray = "DarkGray"

data BlockSide
  = TopSide
  | BottomSide
  | RightSide
  | LeftSide

type Block =
  { square :: Square
  , color  :: Color
  }

type Ball =
  { circle   :: Circle
  , velocity :: Vec
  , color    :: Color
  }

type Sink =
  { square :: Square
  , circle :: Circle
  , color  :: Color
  }

type BallSource =
  { square :: Square
  , circle :: Circle
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

mkBall :: {x :: Number, y :: Number} -> Vec -> Color -> Ball
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
          { x: betweenCells + (blockSide + betweenCells) * toNumber coor.col
          , y: betweenCells + (blockSide + betweenCells) * toNumber coor.row
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

mkBallSource :: BoardCoordinate -> BallSource
mkBallSource coor =
  let topLeft =
        { x: betweenCells + (blockSide + betweenCells) * toNumber coor.col
        , y: betweenCells + (blockSide + betweenCells) * toNumber coor.row
        }
   in { square:
          { topLeft: topLeft
          , side: blockSide
          }
      , circle:
          { center:
              { x: topLeft.x + blockSide / 2.0
              , y: topLeft.y + blockSide / 2.0
              }
          , radius: ballSourceRadius
          }
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

-- | Create an array of the blocks on the given board.
-- |
-- | The blocks are all the board elements with an upper alphabetical character.
generateBlocks :: Array Block
generateBlocks =
  let blocksCells = filter (\(Tuple c _) -> isUpper c) enumBoard
   in toUnfoldable $ map toBlock blocksCells
  where
    toBlock :: Tuple Char BoardCoordinate -> Block
    toBlock (Tuple c coor) = mkBlock coor (charToColor c)

generateBlocksMap :: HashMap BoardCoordinate Block
generateBlocksMap =
  let blocksCells = filter (\(Tuple c _) -> isUpper c) enumBoard
   in HM.fromFoldable $ map toBlock blocksCells
  where
    toBlock :: Tuple Char BoardCoordinate -> Tuple BoardCoordinate Block
    toBlock (Tuple c coor) = Tuple coor (mkBlock coor (charToColor c))

-- | Create an array of the sinks on the given board.
-- |
-- | The sinks are all the board elements with a lower alphabetical character.
generateSinks :: Array Sink
generateSinks =
  let sinksCells = filter (\(Tuple c _) -> isLower c) enumBoard
   in toUnfoldable $ map toSink sinksCells
  where
    toSink :: Tuple Char BoardCoordinate -> Sink
    toSink (Tuple c coor) = mkSink coor (charToColor c)

-- | Create an array of the ball sources on the given board.
-- |
-- | The ball sources are all the board elements with a `@` character.
generateBallSources :: Array BallSource
generateBallSources =
  let ballSourcesCells = filter (\(Tuple c _) -> c == '@') enumBoard
   in toUnfoldable $ map toBallSource ballSourcesCells
  where
    toBallSource :: Tuple Char BoardCoordinate -> BallSource
    toBallSource (Tuple _ coor) = mkBallSource coor

charToColor :: Char -> Color
charToColor c = smallCharToColor (toLower c)
  where
    smallCharToColor 'g' = Green
    smallCharToColor 'r' = Red
    smallCharToColor 'w' = White
    smallCharToColor 'b' = Blue
    smallCharToColor 'y' = Yellow
    smallCharToColor  _  = Gray
