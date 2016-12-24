module Onitama.Card (
    cards
  ) where

import qualified Data.List as L

import           Onitama.Data

-----------------------

-- NOTE: There must be _at least_ 5 cards defined here
--
-- http://catholicmom.com/wp-content/uploads/2016/07/Onitama-3.jpg
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
        , " #@  "
        , "  #  "
        , "     "
        ]
    , parseCard "Boar" [
          "     "
        , "  #  "
        , " #@# "
        , "     "
        , "     "
        ]
    , parseCard "Monkey" [
          "     "
        , " # # "
        , "  @  "
        , " # # "
        , "     "
        ]
    , parseCard "Rooster" [
          "     "
        , "   # "
        , " #@# "
        , " #   "
        , "     "
        ]
    , parseCard "Frog" [
          "     "
        , " #   "
        , "# @  "
        , "   # "
        , "     "
        ]
    , parseCard "Rabbit" [
          "     "
        , "   # "
        , "  @ #"
        , " #   "
        , "     "
        ]
    , parseCard "Cobra" [
          "     "
        , "   # "
        , " #@  "
        , "   # "
        , "     "
        ]
    , parseCard "Eel" [
          "     "
        , " #   "
        , "  @# "
        , " #   "
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
    , parseCard "Sheep" [
          "     "
        , " # # "
        , "  @  "
        , " # # "
        , "     "
        ]
    , parseCard "Crane" [
          "     "
        , "  #  "
        , "  @  "
        , " # # "
        , "     "
        ]
    , parseCard "Mantis" [
          "     "
        , " # # "
        , "  @  "
        , "  #  "
        , "     "
        ]
    , parseCard "Elephant" [
          "     "
        , " # # "
        , " #@# "
        , "     "
        , "     "
        ]
    ]

parseCard :: String -> [[Char]] -> Card
parseCard n =
  Card n .  map (map $ flip L.lookup [('@', CardMiddle), ('#', CardMove)])
