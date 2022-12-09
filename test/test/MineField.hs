{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.MineField where

import Control.Monad.Random (Rand, RandomGen, evalRand, mkStdGen)
import GHC.Arr
import MineField
import Test.Tasty.HUnit
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy.Internal (packChars)
import Data.Aeson.Types
import Data.Maybe

gen = mkStdGen 0

assertIsJust :: HasCallStack => Maybe a -> IO a
assertIsJust Nothing = assertFailure "Value should not be Nothing"
assertIsJust (Just a) = return a

generateMineFieldWithGen :: (Int, Int) -> Int -> Maybe MineField
generateMineFieldWithGen size minesCount =
    evalRand (generateMineField size minesCount) gen

unit_cellNumbers = do
    cellNumbersMediumField2 @=?
        map (cellNumber mediumField2) (indices $ cells mediumField2)

unit_cellLabels = do
    cellLabelsOpenMineResultSmallField1 @=? cellLabels openMineResultSmallField1
    cellLabelsOpenMineResultSmallField2 @=? cellLabels openMineResultSmallField2
    cellLabelsSmallField2 @=? cellLabels smallField2
  where
    cellLabels field = map (cellLabel field) (indices $ cells field)

unit_generateMineFieldHasCorrectMinesCount = do
    mineField <- assertIsJust $ generateMineFieldWithGen (30, 40) 30
    let mineCells = cells mineField
    ((0, 0), (29, 39)) @=? bounds mineCells
    30 @=? width mineField
    40 @=? height mineField
    let countMines =
            sum
                [ fromEnum $ isMine $ cells mineField ! (i, j)
                | i <- [0 .. 29]
                , j <- [0 .. 39]
                ]
    30 @=? countMines

unit_generateMineFieldHasUnopenedFields = do
    mineField <- assertIsJust $ generateMineFieldWithGen (30, 41) 7
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
        mineField1 <- generateMineField (30, 30) 30
        mineField2 <- generateMineField (30, 30) 30
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
    Unopened @=? cellState (cells mediumField1 ! (0, 1))
    let flaggedField = flagCell mediumField1 (0, 1)
    Flagged @=? cellState (cells flaggedField ! (0, 1))
    Unopened @=? cellState (cells flaggedField ! (1, 1))

unit_openFlaggedCell = do
    let flaggedField = flagCell mediumField1 (2, 2)
    Flagged @=? cellState (cells flaggedField ! (2, 2))
    let openedFlaggedField = openCell flaggedField (2, 2)
    flaggedField @=? openedFlaggedField

unit_openOpenedCell = do
    let openedField = openCell mediumField1 (2, 2)
    Opened @=? cellState (cells openedField ! (2, 2))
    let openOpenedField = openCell openedField (2, 2)
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
    assertJsonEqual jsonSmallField2 smallField2
    assertJsonEqual jsonOpenMineResultSmallField2 openMineResultSmallField2
  where
    assertJsonEqual jsonString field = 
        (decode (packChars jsonString) :: Maybe Value) @=? decode (encode field)

unit_isPositionInRange = do
    assertBool "(0, 0) is in range" $ isPositionInRange smallField1 (0, 0)
    assertBool "(1, 1) is in range" $ isPositionInRange smallField1 (1, 1)
    assertBool "(1, 2) is not in range" $ not (isPositionInRange smallField1 (1, 2))
    assertBool "(-1, 0) is not in range" $ not (isPositionInRange smallField1 (-1, 0))

unit_validGenerateMineFieldParameters = do
    assertBool "(2, 3), 6 is ok" $ isJust $ generateMineFieldWithGen (2, 3) 6
    assertBool "(2, 3), 0 is ok" $ isJust $ generateMineFieldWithGen (2, 3) 0
    assertBool "(2, 3), 3 is ok" $ isJust $ generateMineFieldWithGen (2, 3) 3
    assertBool "(2, 3), 7 has too many mines" $ isNothing $ generateMineFieldWithGen (2, 3) 7
    assertBool "(-2, 3), 1 has a negative width" $ isNothing $ generateMineFieldWithGen (-2, 3) 1
    assertBool "(1000, 1000), 7 is too big" $ isNothing $ generateMineFieldWithGen (1000, 1000) 7

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

jsonSmallField2 = "{\"cellLabels\": [\"UU\",\"FU\"], \"gameState\": \"Running\", \"countMinesLeft\": 0}"

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

jsonOpenMineResultSmallField2 = "{\"gameState\": \"Lost\", \"cellLabels\": [\"11\",\"FM\"], \"countMinesLeft\": 0}"

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
