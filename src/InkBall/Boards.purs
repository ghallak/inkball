module InkBall.Boards
  ( Board
  , board
  ) where

import Prelude

import Data.List (List, fromFoldable)

type Board = List (List Char)

board :: Board
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
