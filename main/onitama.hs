{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forever, join)

import qualified Data.Char as C
import qualified Data.List as L
import           Data.Monoid ((<>))

import           Onitama.Card
import           Onitama.Data
import           Onitama.Game

import           System.Exit (exitSuccess)
import qualified System.IO as IO

import           Text.Read (readMaybe)

-----------------------

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  forever $ do
    r <- newGame cards >>= runGame
      Blue
      (let
        go p cs = do
          IO.putStr $ drawPlayer p <> " move: "
          IO.hFlush IO.stdout
          l <- IO.getLine
          maybe (IO.putStrLn "Invalid command" >> go p cs) pure . parseMove cs $ l
        in
          go)
      (\e -> IO.putStrLn $ case e of
        NoPieceSelected ->
          "No piece selected, try again"
        InvalidPiece ->
          "You've selected the other players piece"
        InvalidMoveForCard ->
          "Invalid move, try again"
        InvalidCapture ->
          "Whoops, you've tried to take your own piece"
        PositionTaken ->
          "This position is already taken"
        )
      (IO.putStrLn . drawGame)
    IO.putStrLn $ drawGameOver r
    IO.putStrLn "Play again? (y/n)"
    l <- IO.getLine
    case l of
      'y' : _ ->
        pure ()
      _ ->
        exitSuccess

parseMove :: [Card] -> String -> Maybe Move
parseMove cards' s =
  case s of
    x1 : ',' : y1 : ' ' : x2 : ',' : y2 : ' ' : c ->
      Move
        <$> L.find ((==) (fmap C.toLower c) . fmap C.toLower . cardName) cards'
        <*> (join $ newCoord <$> readMaybe [x1] <*> readMaybe [y1])
        <*> (join $ newCoord <$> readMaybe [x2] <*> readMaybe [y2])
    _ ->
      Nothing

drawGame :: Game -> String
drawGame g =
  let
    drawCardRow :: Player -> Game -> [String]
    drawCardRow p' =
      foldr (\a -> drawJoin "   " (drawCard . snd $ a)) [] . filter ((==) p' . fst) . gameCards
  in
    L.intercalate "\n" . fmap (L.intercalate "\n") $ [
        drawCardRow Red g
      , []
      , drawJoin "   " (drawGrid drawGameSquare $ gameBoard g) (drawCard $ gameSpareCard g)
      , drawCardRow Blue g
      , []
      ]

drawJoin :: String -> [String] -> [String] -> [String]
drawJoin sep as bs =
  let
    m = max (length as) (length bs)
    e x = x <> replicate (m - length x) (replicate (L.maximum . fmap L.length $ "" : x) ' ')
  in
    L.zipWith (\a b -> a <> sep <> b) (e as) (e bs)

drawGameOver :: GameOver -> String
drawGameOver g =
  L.intercalate "\n" . fmap (L.intercalate "\n") $ [
      ["Game over, " <> drawPlayer (gameOverWinner g) <> " won!"]
    , drawGrid drawGameSquare (gameOverBoard g)
    ]

drawPlayer :: Player -> String
drawPlayer p =
  case p of
    Red ->
      "Red"
    Blue ->
      "Blue"

drawGrid :: (a -> Char) -> Grid a -> [String]
drawGrid f g =
  flip map g $ fmap (maybe ' ' f)

drawCard :: Card -> [String]
drawCard (Card n g) =
  n : fmap (fmap (maybe ' ' drawCardSquare)) g

drawCardSquare :: CardSquare -> Char
drawCardSquare cs =
  case cs of
    CardMiddle ->
      '@'
    CardMove ->
      '#'

drawGameSquare :: GameSquare -> Char
drawGameSquare s =
  case s of
    (Red, Minion) ->
      'r'
    (Red, Master) ->
      'R'
    (Blue, Minion) ->
      'b'
    (Blue, Master) ->
      'B'
