onitama
=======

A silly little implementation of
[onitama](https://boardgamegeek.com/boardgame/160477/onitama) in Haskell.

## Screenshot

```
Tiger   Monkey        +---+---+---+---+---+
-----   -----         |   |   |   |   |   |
--#--   -#-#-       4 | r |   | R | r |   |
--@--   --@--         |   |   |   |   |   |
-----   -#-#-         +---+---+---+---+---+
--#--   -----         |   |   |   |   |   |
                    3 |   | r |   | r |   |
Eel                   |   |   |   |   |   |
-----                 +---+---+---+---+---+
-#---                 |   |   |   |   |   |
--@#-               2 |   |   |   |   |   |
-#---                 |   |   |   |   |   |
-----                 +---+---+---+---+---+
                      |   |   |   |   |   |
Frog    Ox          1 |   |   | b | b |   |
-----   -----         |   |   |   |   |   |
-#---   --#--         +---+---+---+---+---+
#-@--   --@#-         |   |   |   |   |   |
---#-   --#--       0 | b |   | B |   | b |
-----   -----         |   |   |   |   |   |
                      +---+---+---+---+---+
                        a   b   c   d   e
```

## Development

1. Install [ghc](https://github.com/ambiata/mafia/blob/master/doc/ghc.md)
2. Install [cabal](https://github.com/ambiata/mafia/blob/master/doc/cabal.md)
3. Build

    ```
    ./mafia build
    ```
4. `./dist/build/onitama-cli/onitama-cli`
