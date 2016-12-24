{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forever, join, when)

import qualified Data.Char as C
import qualified Data.List as L
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))

import           System.Exit (exitSuccess)
import qualified System.IO as IO
import           System.Random.Shuffle (shuffleM)

import           Text.Read (readMaybe)


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  forever $ do
    r <- newGame >>= runGame
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

data InvalidMove =
    NoPieceSelected
  | InvalidPiece
  | InvalidMoveForCard
  | InvalidCapture
  | PositionTaken
  deriving (Eq, Show)

-----------------------

newCoord :: Int -> Int -> Maybe Coord
newCoord x y =
  if x >= 0 && x < 5 && y >=0 && y < 5 then
    pure (x, y)
  else
    Nothing

newGame :: IO Game
newGame = do
  x <- shuffleM cards
  case x of
    c1 : c2 : c3 : c4 : c5 : _ ->
      pure $ Game ([(,) Red (flipCard c1), (,) Red (flipCard c2)] <> [(,) Blue c3, (,) Blue c4]) c5 initialBoard
    _ ->
      fail "Could not select 5 cards, which is inconceivable"

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

  when (not $ isValidCardMove m) $
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
    (mx, gcs) = L.partition ((==) ((,) p (cardName . moveCard $ m)) . fmap cardName) (gameCards g)
  c <- case mx of
    c : [] ->
      pure . snd $ c
    _ ->
      Left InvalidMoveForCard

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

isValidCardMove :: Move -> Bool
isValidCardMove m =
  L.elem (distance (moveSourceCoord m) (moveTargetCoord m)) . validCardMoves $ moveCard m

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

-----------------------

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

-----------------------

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

-----------------------

-- NOTE: There must be _at least_ 5 cards defined here
cards :: [Card]
cards =
  [
      parseCard "Ox" [
          "     "
        , "  #  "
        , "  @# "
        , "  #  "
        , "     "
        ]
    , parseCard "Horse" [
          "     "
        , "  #  "
        , "  @# "
        , "  #  "
        , "     "
        ]
    , parseCard "Tiger" [
          "  #  "
        , "     "
        , "  @  "
        , "  #  "
        , "     "
        ]
    , parseCard "Crab" [
          "     "
        , "  #  "
        , "# @ #"
        , "     "
        , "     "
        ]
    , parseCard "Dragon" [
          "     "
        , "#   #"
        , "  @  "
        , " # # "
        , "     "
        ]
    ]

parseCard :: String -> [[Char]] -> Card
parseCard n =
  Card n .  map (map $ flip L.lookup [('@', CardMiddle), ('#', CardMove)])

