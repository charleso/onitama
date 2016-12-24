module Onitama.Card (
    cards
  ) where

import qualified Data.List as L

import           Onitama.Data

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
