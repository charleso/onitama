{-# LANGUAGE OverloadedStrings #-}
module Onitama.Game (
    InvalidMove (..)
  , newGame
  , runGame
  ) where

import           Control.Monad (join, when)

import qualified Data.Char as C
import qualified Data.List as L
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))

import           Onitama.Data

import           System.Random.Shuffle (shuffleM)

-----------------------

data InvalidMove =
    NoPieceSelected
  | InvalidPiece
  | InvalidMoveForCard
  | InvalidCapture
  | PositionTaken
  | CardNotFound
  deriving (Eq, Show)

-----------------------

newGame :: [Card] -> IO Game
newGame cards' = do
  x <- shuffleM cards'
  case x of
    c1 : c2 : c3 : c4 : c5 : _ ->
      pure $ Game ([(,) Red (flipCard c1), (,) Red (flipCard c2)] <> [(,) Blue c3, (,) Blue c4]) c5 initialBoard
    _ ->
      fail "Could not select 5 cards, which is inconceivable"

runGame ::
  Monad m =>
  Player ->
  (Player -> [Card] -> m Move) ->
  (InvalidMove -> m ()) ->
  (Game -> m ()) ->
  Game ->
  m GameOver
runGame p newMove' printError printGame' g1 = do
  printGame' g1
  m <- newMove' p (fmap snd . filter ((==) p . fst) $ gameCards g1)
  case move p m g1 of
    Left e -> do
      printError e
      runGame p newMove' printError printGame' g1
    Right (Right g2) ->
      runGame (switchPlayer p) newMove' printError printGame' g2
    Right (Left go) ->
      pure go

initialBoard :: Grid GameSquare
initialBoard =
  [
      fmap (Just . (,) Red) $ [Minion, Minion, Master, Minion, Minion]
    , [Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing]
    , fmap (Just . (,) Blue) $ [Minion, Minion, Master, Minion, Minion]
    ]

swapGridValue :: Coord -> Maybe a -> Grid a -> (Maybe a, Grid a)
swapGridValue (x, y) a g =
  let
    (yl, y' : yr) = splitAt y . reverse $ g
    (xl, x' : xr) = splitAt x y'
  in
    (x', reverse yr <> [xl <> [a] <> xr] <> reverse yl)

move :: Player -> Move -> Game -> Either InvalidMove (Either GameOver Game)
move p m g = do

  let
    (mx, gcs) = L.partition (\(p', c') -> p == p' && isCardSelected (moveCard m) c') (gameCards g)
  c <- case mx of
    c : [] ->
      pure . snd $ c
    _ ->
      Left CardNotFound

  when (not $ isValidCardMove m c) $
    Left InvalidMoveForCard

  let
    (x, gb1) = swapGridValue (moveSourceCoord m) Nothing (gameBoard g)
  case x of
    Nothing ->
      Left NoPieceSelected
    Just (pl, _) ->
      when (pl /= p) $
        Left InvalidPiece

  let
    (target, gb2) = swapGridValue (moveTargetCoord m) x gb1

  when (fmap fst target == Just p) $
    Left InvalidCapture

  let
    winningCoording p' =
      case p' of
        Red ->
          (0, 2)
        Blue ->
          (4, 2)

  pure $
    if target == Just (switchPlayer p, Master) || moveTargetCoord m == winningCoording p then
      Left $ GameOver
        p
        gb2
    else
      pure $ Game
        ((,) p (gameSpareCard g) : gcs)
        (flipCard c)
        gb2

isValidCardMove :: Move -> Card -> Bool
isValidCardMove m =
  L.elem (distance (moveSourceCoord m) (moveTargetCoord m)) . validCardMoves

distance :: Coord -> Coord -> Coord
distance (x1, y1) (x2, y2) =
  (x2 - x1, y2 - y1)

validCardMoves :: Card -> [Coord]
validCardMoves =
  let
    middle = (2, 2)
  in
    id
      . fmap (distance middle)
      . L.delete middle
      . catMaybes
      . join
      . L.zipWith (\y -> L.zipWith (\x m -> fmap (const (x, y)) m) [0..]) [0..]
      . reverse
      . cardGrid

isCardSelected :: CardSelect -> Card -> Bool
isCardSelected cs c =
  let
    toLower = fmap C.toLower
  in
    (toLower . renderCardSelect) cs == (toLower . cardName) c

flipCard :: Card -> Card
flipCard c =
  c { cardGrid = reverse $ cardGrid c }

switchPlayer :: Player -> Player
switchPlayer p =
  case p of
    Red ->
      Blue
    Blue ->
      Red
