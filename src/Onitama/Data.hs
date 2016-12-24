{-# LANGUAGE OverloadedStrings #-}
module Onitama.Data (
    Game (..)
  , GameOver (..)
  , Board
  , Grid
  , Coord
  , Card (..)
  , CardSquare (..)
  , GameSquare
  , Move (..)
  , Player (..)
  , Piece (..)
  , newCoord
  ) where

-----------------------

data Game =
  Game {
      gameCards :: [(Player, Card)]
    , gameSpareCard :: Card
    , gameBoard :: Board
    }

data GameOver =
  GameOver {
      gameOverWinner :: Player
    , gameOverBoard :: Board
    }

type Board = Grid GameSquare

type Grid a = [[Maybe a]]

type Coord = (Int, Int)

data Card =
  Card {
      cardName :: String
    , cardGrid :: Grid CardSquare
    } deriving (Eq, Show)

data CardSquare =
    CardMiddle
  | CardMove
  deriving (Eq, Show)

type GameSquare = (Player, Piece)

data Move =
  Move {
      moveCard :: Card
    , moveSourceCoord :: Coord
    , moveTargetCoord :: Coord
    } deriving (Eq, Show)

data Player =
    Red
  | Blue
  deriving (Eq, Show)

data Piece =
    Minion
  | Master
  deriving (Eq, Show)

-----------------------

newCoord :: Int -> Int -> Maybe Coord
newCoord x y =
  if x >= 0 && x < 5 && y >=0 && y < 5 then
    pure (x, y)
  else
    Nothing
