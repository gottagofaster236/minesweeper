{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.MineField where

import Control.Monad.Random (Rand, RandomGen, evalRand, mkStdGen)
import GHC.Arr
import MineField
import Test.Tasty.HUnit
import Data.Aeson (encode)
import Data.ByteString.Lazy.Internal (unpackChars)

gen = mkStdGen 0

unit_cellNumbers = do
    cellNumbersMediumField2 @=?
        map (cellNumber mediumField2) (indices $ cells mediumField2)

unit_cellLabels = do
    cellLabelsOpenMineResultSmallField1 @=? cellLabels openMineResultSmallField1
    cellLabelsOpenMineResultSmallField2 @=? cellLabels openMineResultSmallField2
    cellLabelsSmallField2 @=? cellLabels smallField2
  where
    cellLabels field = map (cellLabel field) (indices $ cells field)

unit_generateMineFieldHasCorrectMinesCount :: IO ()
unit_generateMineFieldHasCorrectMinesCount = do
    let mineField = evalRand (generateMineField (50, 100) 30) gen
    let mineCells = cells mineField
    ((0, 0), (49, 99)) @=? bounds mineCells
    50 @=? width mineField
    100 @=? height mineField
    let countMines =
            sum
                [ fromEnum $ isMine $ cells mineField ! (i, j)
                | i <- [0 .. 49]
                , j <- [0 .. 99]
                ]
    30 @=? countMines

unit_generateMineFieldHasUnopenedFields = do
    let mineField = evalRand (generateMineField (123, 321) 7) gen
    let allFieldsUnopened =
            all (\cell -> cellState cell == Unopened) (elems $ cells mineField)
    assertBool "Fields should be unopened in the beginning" allFieldsUnopened

unit_generateMineFieldHasDifferentResults = do
    assertBool
        "Random fields should be different"
        (evalRand compareTwoBigFields gen)
  where
    compareTwoBigFields :: (RandomGen g) => Rand g Bool
    compareTwoBigFields = do
        mineField1 <- generateMineField (500, 500) 500
        mineField2 <- generateMineField (500, 500) 500
        return $ mineField1 /= mineField2

unit_openCellOneByOne = do
    assertBool
        "Field should be unopened in the beginning"
        (cellState (cells oneByOneField ! (0, 0)) == Unopened)
    let openedField = openCell oneByOneField (0, 0)
    Opened @=? cellState (cells openedField ! (0, 0))

unit_openSeveralCells = do
    openResultSmallField1 @=? openCell smallField1 (0, 1)
    openResultMediumField1 @=? openCell mediumField1 (2, 2)
    openResultMediumField2 @=? openCell mediumField2 (0, 1)
    openResultMediumField3 @=? openCell mediumField3 (0, 0)

unit_flagCell = do
    Unopened @=? cellState (cells defaultField ! (2, 3))
    let flaggedField = flagCell defaultField (2, 3)
    Flagged @=? cellState (cells flaggedField ! (2, 3))
    Unopened @=? cellState (cells flaggedField ! (2, 4))

unit_openFlaggedCell = do
    let flaggedField = flagCell defaultField (5, 5)
    Flagged @=? cellState (cells flaggedField ! (5, 5))
    let openedFlaggedField = openCell flaggedField (5, 5)
    flaggedField @=? openedFlaggedField

unit_openOpenedCell = do
    let openedField = openCell defaultField (5, 5)
    Opened @=? cellState (cells openedField ! (5, 5))
    let openOpenedField = openCell openedField (5, 5)
    openedField @=? openOpenedField

unit_flagOpenedCell = do
    let openedField = openCell oneByOneField (0, 0)
    Opened @=? cellState (cells openedField ! (0, 0))
    let flaggedOpenedField = flagCell openedField (0, 0)
    Opened @=? cellState (cells flaggedOpenedField ! (0, 0))

unit_flagFlaggedCell = do
    let flaggedField = flagCell oneByOneField (0, 0)
    Flagged @=? cellState (cells flaggedField ! (0, 0))
    let flaggedFlaggedField = flagCell flaggedField (0, 0)
    Unopened @=? cellState (cells flaggedFlaggedField ! (0, 0))

unit_openCellWithMine = do
    openMineResultSmallField1 @=? openCell smallField1 (1, 1)
    openMineResultSmallField2 @=? openCell smallField2 (1, 1)

unit_countMinesLeft = do
    1 @=? countMinesLeft smallField1
    let oneFlag = flagCell smallField1 (0, 0)
    0 @=? countMinesLeft oneFlag
    let twoFlags = flagCell oneFlag (0, 1)
    -1 @=? countMinesLeft twoFlags
    let threeFlags = flagCell twoFlags (1, 1)
    -2 @=? countMinesLeft threeFlags
    let twoFlagsAgain = flagCell threeFlags (0, 1)
    -1 @=? countMinesLeft twoFlagsAgain

unit_getGameState = do
    Won @=? getGameState winField1
    Won @=? getGameState winField2
    Won @=? getGameState winField3
    Lost @=? getGameState loseField1
    Lost @=? getGameState loseField2
    Running @=? getGameState runningField1
    Running @=? getGameState runningField2

unit_mineFieldJson = do
    jsonSmallField2 @=? unpackChars (encode smallField2)
    jsonOpenMineResultSmallField2 @=? unpackChars (encode openMineResultSmallField2)

defaultField = evalRand (generateMineField (10, 10) 10) gen

oneByOneField =
    MineField
        { cells =
              array
                  ((0, 0), (1 - 1, 1 - 1))
                  [((0, 0), Cell {cellState = Unopened, isMine = False})]
        }

-- ▢▢
-- ▢💣
smallField1 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Unopened, isMine = False})
                  , ((0, 1), Cell {cellState = Unopened, isMine = False})
                  , ((1, 0), Cell {cellState = Unopened, isMine = False})
                  , ((1, 1), Cell {cellState = Unopened, isMine = True})
                  ]
        }

-- ▢▢
-- 1💣
openResultSmallField1 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Unopened, isMine = False})
                  , ((0, 1), Cell {cellState = Opened, isMine = False})
                  , ((1, 0), Cell {cellState = Unopened, isMine = False})
                  , ((1, 1), Cell {cellState = Unopened, isMine = True})
                  ]
        }

-- 11
-- 1💥
openMineResultSmallField1 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Opened, isMine = False})
                  , ((0, 1), Cell {cellState = Opened, isMine = False})
                  , ((1, 0), Cell {cellState = Opened, isMine = False})
                  , ((1, 1), Cell {cellState = Opened, isMine = True})
                  ]
        }

cellLabelsOpenMineResultSmallField1 = "111M"

-- ▢🚩
-- ▢💣
smallField2 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Unopened, isMine = False})
                  , ((0, 1), Cell {cellState = Unopened, isMine = False})
                  , ((1, 0), Cell {cellState = Flagged, isMine = False})
                  , ((1, 1), Cell {cellState = Unopened, isMine = True})
                  ]
        }

cellLabelsSmallField2 = "UUFU"

jsonSmallField2 = "{\"cellLabels\":[\"UU\",\"FU\"],\"gameState\":\"Running\"}"

-- 1🚩
-- 1💥
openMineResultSmallField2 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Opened, isMine = False})
                  , ((0, 1), Cell {cellState = Opened, isMine = False})
                  , ((1, 0), Cell {cellState = Flagged, isMine = False})
                  , ((1, 1), Cell {cellState = Opened, isMine = True})
                  ]
        }

jsonOpenMineResultSmallField2 = "{\"cellLabels\":[\"11\",\"FM\"],\"gameState\":\"Lost\"}"

cellLabelsOpenMineResultSmallField2 = "11FM"

-- ▢▢▢
-- ▢▢▢
-- ▢▢▢
mediumField1 =
    MineField
        { cells =
              listArray
                  ((0, 0), (3 - 1, 3 - 1))
                  (repeat Cell {cellState = Unopened, isMine = False})
        }

-- ___
-- ___
-- ___
openResultMediumField1 =
    MineField
        { cells =
              listArray
                  ((0, 0), (3 - 1, 3 - 1))
                  (repeat Cell {cellState = Opened, isMine = False})
        }

-- ▢▢▢
-- ▢▢💣
-- ▢▢▢
mediumField2 =
    MineField
        { cells =
              cells mediumField1 //
              [((2, 1), Cell {cellState = Unopened, isMine = True})]
        }

-- _1▢
-- _1💣
-- _1▢
openResultMediumField2 =
    MineField
        { cells =
              cells openResultMediumField1 //
              [ ((2, 0), Cell {cellState = Unopened, isMine = False})
              , ((2, 1), Cell {cellState = Unopened, isMine = True})
              , ((2, 2), Cell {cellState = Unopened, isMine = False})
              ]
        }

cellNumbersMediumField2 = [0, 0, 0, 1, 1, 1, 1, 0, 1]

-- ▢▢🚩
-- ▢🚩▢
-- 🚩▢▢
mediumField3 =
    MineField
        { cells =
              cells mediumField1 //
              [ ((2, 0), Cell {cellState = Flagged, isMine = False})
              , ((1, 1), Cell {cellState = Flagged, isMine = False})
              , ((0, 2), Cell {cellState = Flagged, isMine = False})
              ]
        }

-- __🚩
-- _🚩▢
-- 🚩▢▢
openResultMediumField3 =
    MineField
        { cells =
              cells mediumField3 //
              [ ((0, 0), Cell {cellState = Opened, isMine = False})
              , ((1, 0), Cell {cellState = Opened, isMine = False})
              , ((0, 1), Cell {cellState = Opened, isMine = False})
              ]
        }

-- 11
-- 1💣
winField1 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Opened, isMine = False})
                  , ((0, 1), Cell {cellState = Opened, isMine = False})
                  , ((1, 0), Cell {cellState = Opened, isMine = False})
                  , ((1, 1), Cell {cellState = Unopened, isMine = True})
                  ]
        }

-- 11
-- 1🚩
winField2 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Opened, isMine = False})
                  , ((0, 1), Cell {cellState = Opened, isMine = False})
                  , ((1, 0), Cell {cellState = Opened, isMine = False})
                  , ((1, 1), Cell {cellState = Flagged, isMine = True})
                  ]
        }

-- 💣2
-- 2🚩
winField3 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Unopened, isMine = True})
                  , ((0, 1), Cell {cellState = Opened, isMine = False})
                  , ((1, 0), Cell {cellState = Opened, isMine = False})
                  , ((1, 1), Cell {cellState = Flagged, isMine = True})
                  ]
        }

-- 11
-- 1💥
loseField1 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Opened, isMine = False})
                  , ((0, 1), Cell {cellState = Opened, isMine = False})
                  , ((1, 0), Cell {cellState = Opened, isMine = False})
                  , ((1, 1), Cell {cellState = Opened, isMine = True})
                  ]
        }

-- 🚩1
-- 1💥
loseField2 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Flagged, isMine = False})
                  , ((0, 1), Cell {cellState = Opened, isMine = False})
                  , ((1, 0), Cell {cellState = Opened, isMine = False})
                  , ((1, 1), Cell {cellState = Opened, isMine = True})
                  ]
        }

-- 1▢
-- 1💣
runningField1 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Opened, isMine = False})
                  , ((0, 1), Cell {cellState = Opened, isMine = False})
                  , ((1, 0), Cell {cellState = Unopened, isMine = False})
                  , ((1, 1), Cell {cellState = Unopened, isMine = True})
                  ]
        }

-- 1▢
-- 1🚩
runningField2 =
    MineField
        { cells =
              array
                  ((0, 0), (2 - 1, 2 - 1))
                  [ ((0, 0), Cell {cellState = Opened, isMine = False})
                  , ((0, 1), Cell {cellState = Opened, isMine = False})
                  , ((1, 0), Cell {cellState = Unopened, isMine = False})
                  , ((1, 1), Cell {cellState = Flagged, isMine = True})
                  ]
        }
