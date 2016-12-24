import           Control.Monad (forever, join)

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
  IO.putStrLn logo
  forever $ do
    r <- newGame cards >>= runGame
      Blue
      (let
        go p cs = do
          IO.putStr $ drawPlayer p <> " move: "
          IO.hFlush IO.stdout
          l <- IO.getLine
          maybe (IO.putStrLn "Invalid command" >> go p cs) pure . parseMove $ l
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
        CardNotFound ->
          "Could not find the selected card"
        )
      (\g -> IO.putStrLn "" >> IO.putStrLn (drawGame g))
    IO.putStrLn $ drawGameOver r
    IO.putStrLn "Play again? (y/n)"
    l <- IO.getLine
    case l of
      'y' : _ ->
        pure ()
      _ ->
        exitSuccess

parseMove :: String -> Maybe Move
parseMove s =
  case s of
    x1 : ',' : y1 : ' ' : x2 : ',' : y2 : ' ' : c ->
      Move
        <$> (pure . CardSelect) c
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
    left = join [
        drawCardRow Red g
      , drawCard $ gameSpareCard g
      , drawCardRow Blue g
      ]
  in
    L.intercalate "\n" $
      drawJoin "   " left (drawGrid drawGameSquare $ gameBoard g)

drawJoin :: String -> [String] -> [String] -> [String]
drawJoin sep as bs =
  let
    pad x s = s <> replicate (flip (-) (length s) . L.maximum . fmap L.length $ "" : x) ' '
    m = max (length as) (length bs)
    e x = fmap (pad x) x <> replicate (m - length x) (pad x "")
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
  (flip (<>) ["+---+---+---+---+---+"]) $ g >>=
    flip (drawJoin "") ["+", "|", "|", "|"]
      . foldr (drawJoin "") []
      . fmap (\x -> ["+---", "|   ", ['|', ' ', maybe ' ' f x, ' '], "|  "])

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

logo :: String
logo =
  unlines [
      "                       `,::::,`                                                      `.`"
    , "            ::      .::::::::::::.                                                  `::::::"
    , "           :::    ::::::::::::::::::                                                ,:::::::"
    , "          ::.   .:::::::::::::::::::::                                                :::::::"
    , "         ::    ::::::::::::::::::::::::                                               `::::::"
    , "        ::`   ,:::::::::::::::::::::::::                                              `::::::"
    , "        ::   `::::::::,`    `::::::::::::                              .               ::::::"
    , "       ::    :::::::          `::::::::::                             :::              ::::::`"
    , "       ::   :::::::             ::::::::::                           :::::             ::::::."
    , "      ::   ::::::,               :::::::::,                         `::::::            :::::::"
    , "      ::   ::::::                 :::::::::                         .::::::,           :::::::              `,:::::::::"
    , "      ::  ,:::::                  ::::::::::                         :::::::           :::::::         .:::::::::::::::,"
    , "     ,:.  .::::                    :::::::::                          :::::::          :::::::    `::::::::::::::::::::"
    , "     ::    :::                     :::::::::`                         :::::::          :::::::`,:::::::::::::::::::::,"
    , "     ::    `:                       :::::::::                         :::::::,         ::::::::::::::::::::::::::::,"
    , "     ::                             :::::::::                         :::::::         `::::::::::::::::::::::::,`"
    , "     ::                             :::::::::                          :::::       .:::::::::::::::::::,."
    , "     ::                             :::::::::                                   :::::::::::::::,.`"
    , "    `::                             :::::::::.                                 :::::::,::::::::"
    , "    .::                             .::::::::,                            .`  .::::,   ::::::::                                                                     ::,,"
    , "    .::                             .::::::::,   .      `::::.           :::,  ..      ::::::::           ,:::::::                  ::::         ,:::`            .::::``:"
    , "    ,::                             .::::::::`  :::     :::::::          ::::`        `::::::::         :::::::::::.       ::      :::::,      ::::::::          :::::::::.:`"
    , "    ,::                             .::::::::`  :::     ::::::::         :::::        `::::::::        :::::::::::::`     :::     :::::::`    ::::::::::        ::::::::::::::"
    , "    ,::                             .::::::::`  :::    :::::::::.        ::::::       .::::::::       :::::::::::::::     ::::   ,:  :::::   ::::::::::::      :::::  ::::::::`"
    , "    ,::.                            :::::::::  .:::   :::.:::::::        ::::::`      .::::::::       :::::` ::::::::     ::::   :   `::::` .::::::::::::     `::::     :::::::"
    , "    ,:::                            :::::::::  ::::  .::: :::::::.       :::::::      ,::::::::`    `:::::   ::::::::     ::::` :.    ::::: ::::, ::::::::    :::::       :::::"
    , "    .:::                            :::::::::  ::::  :::  ::::::::       :::::::,     ,::::::::.    .::::    :::::::::    :::::.:     :::::::::`  ::::::::    ::::.        ::::."
    , "     :::`                           ::::::::,  :::: :::   ::::::::      .::::::::     :::::::::,    ::::,    :::::::::    :::::::     :::::::::   ::::::::    ::::         :::::"
    , "     ::::                          .::::::::   ::::,::    ::::::::      :::::::::`    :::::::::,   `::::     :::::::::,   :::::::     ::::::::.   ::::::::.  .::::         :::::"
    , "     ::::                          :::::::::  ,::::::`    ::::::::      :::::::::,    ::::::::::   ,::::     ::::::::::   ::::::`     ::::::::,   ,::::::::  ,::::         :::::`"
    , "     :::::                         :::::::::  :::::::     ::::::::`     :::::::::,    ::::::::::   .:::,     ::::::::::   ::::::.     :::::::::   .::::::::  :::::         ::::::"
    , "     `::::,                       :::::::::   ::::::      ::::::::.     :::::::::,    ::::::::::   `:::,     ::::::::::   :::::::     :::::::::   `::::::::  :::::        `::::::"
    , "      :::::                       :::::::::   ::::::      :::::::::    `:::::::::.    ::::::::::    :::,     ::::::::::   :::::::     :::::::::    ::::::::  :::::         ::::::"
    , "      ,:::::                     :::::::::.   ::::::      ,::::::::    ::::::::::     ,:::::::::   .:::,     ,:::::::::   :::::::,    :::::::::    ::::::::  :::::       .:::::::"
    , "       ::::::.                  ::::::::::    :::::,      `::::::::    ::::::::::     ,:::::::::   .::::     ,:::::::::   ::::::::    :::::::::    ::::::::  ,::::       ::::::::"
    , "        :::::::                ,:::::::::`    ::::::       :::::::::  `::::::::::     .:::::::::    ::::    `::::::::::  .::::::::   `:::::::::`   ::::::::. `::::.`    :::::::::"
    , "         ::::::::             :::::::::::     `:::::       ,::::::::   ::::::::::     .:::::::::   `::::::,::::::::::::   ::::::::   ::::::::::`   ::::::::,  :::::.,:,::::::::::`"
    , "          :::::::::,        ,:::::::::::       :::::`       :::::::::  ,::::::::.     `:::::::::    :::::::::::::::::::`    ::::::`   :::::::::`   :::::::::  :::::::::::::::::::,"
    , "           ::::::::::::::::::::::::::::         ::::         ::::::::   ::::::::       :::::::::     ::::::::::::`:::::,     ::::::    `:::::::    :::::::::   :::::::::::::::::::"
    , "            ::::::::::::::::::::::::::                       .:::::::`  ::::::::        ::::::::      ,::::::::: `::::::     ::::::     :::::::    :::::::::    ::::::::::::::::::"
    , "             ,:::::::::::::::::::::::                         `::::::   `:::::::         :::::::       `:::::::   ::::::      .::::      ::::::    ,::::::::     ,::::::::: ::::::"
    , "               :::::::::::::::::::::                            ::::     ::::::.          :::::          :::::    ::::::        .:.       `:::     ,::::::::       ::::::    :::::"
    , "                 :::::::::::::::::                               `,       :::::`            .:                     `:::                            ,::::::::                 .::::"
    , "                   ,:::::::::::`                                           .,,`                                                                    .::::::::                  ::::"
    , "                                                                                                                                                   .::::::::                   ,,"
    , "                                                                                                                                                   .::::::::"
    , "                                                                                                                                                   ,::::::::"
    , "                                                                                                                                                   .::::::::"
    , "                                                                                                                                                   ::::::::"
    , "                                                                                                                                                   :::::,,"
    , "                                                                                                                                                   :::"
    , "                                                                                                                                                   :::"
    , "                                                                                                                                                    :"
    ]
