{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.MineField where

import MineField
import Test.Tasty.HUnit
import Control.Monad.Random
    ( Rand, RandomGen, evalRand, mkStdGen, StdGen )
import GHC.Arr

gen :: StdGen
gen = mkStdGen 0

unit_generateMineFieldHasCorrectMinesCount = do
        let mineField = evalRand (generateMineField (50, 100) 30) gen
        let mineCells = cells mineField
        ((0, 0), (49, 99)) @=? bounds mineCells
        50 @=? width mineField
        100 @=? height mineField
        let countMines = sum [boolToInt $ isMine $ cells mineField!(i, j) | i <- [0..49], j <- [0..99]]
        30 @=? countMines
    where
        boolToInt :: Bool -> Int
        boolToInt b = if b then 1 else 0

unit_generateMineFieldHasUnopenedFields = do
    let mineField = evalRand (generateMineField (123, 321) 7) gen
    let allFieldsUnopened = all (\cell -> cellState cell == Unopened) (elems $ cells mineField)
    assertBool "Fields should be unopened in the beginning" allFieldsUnopened

unit_generateMineFieldHasDifferentResults = do
        assertBool "Random fields should be different" (evalRand compareTwoBigFields gen)
    where
        compareTwoBigFields :: (RandomGen g) => Rand g Bool
        compareTwoBigFields = do
            mineField1 <- generateMineField (500, 500) 500
            mineField2 <- generateMineField (500, 500) 500
            return $ mineField1 /= mineField2

unit_openCellOneByOne = do
    assertBool "Field should be unopened in the beginning" (cellState (cells oneByOneField!(0, 0)) == Unopened)
    let openedField = openCell oneByOneField (0, 0)
    Opened @=? cellState (cells openedField!(0, 0))

unit_openSeveralCells = do
    expectedOpenResult1 @=? openCell smallField1 (0, 1)
    expectedOpenResult2 @=? openCell smallField2 (0, 1)
    expectedOpenResult3 @=? openCell smallField3 (0, 1)

unit_flagCell = do
    Unopened @=? cellState (cells defaultField!(2, 3))
    let flaggedField = flagCell defaultField (2, 3)
    Flagged @=? cellState (cells flaggedField!(2, 3))
    Unopened @=? cellState (cells flaggedField!(2, 4))

unit_openFlaggedCell = do
    let flaggedField = flagCell defaultField (5, 5)
    Flagged @=? cellState (cells flaggedField!(5, 5))
    let openedFlaggedField = openCell flaggedField (5, 5)
    flaggedField @=? openedFlaggedField

unit_flagOpenedCell = do
    let openedField = openCell oneByOneField (0, 0)
    Opened @=? cellState (cells openedField!(0, 0))
    let flaggedOpenedField = flagCell openedField (0, 0)
    openedField @=? flaggedOpenedField

unit_openCellWithMine = do
    expectedMineOpenResult1 @=? openCell smallField1 (1, 1)
    expectedMineOpenResult2 @=? openCell smallField2 (1, 1)

defaultField :: MineField
defaultField = evalRand (generateMineField (10, 10) 10) gen


oneByOneField :: MineField
oneByOneField = MineField { cells = array ((0, 0), (1 - 1, 1 - 1)) [
        ((0, 0), Cell { cellState = Unopened, isMine = False})
    ]
}

-- 00
-- 0X
smallField1 :: MineField
smallField1 = MineField { cells = array ((0, 0), (2 - 1, 2 - 1)) [
        ((0, 0), Cell { cellState = Unopened, isMine = False}),
        ((0, 1), Cell { cellState = Unopened, isMine = False}),
        ((1, 0), Cell { cellState = Unopened, isMine = False}),
        ((1, 1), Cell { cellState = Unopened, isMine = True})
    ]
}

-- __
-- _X
expectedOpenResult1 :: MineField
expectedOpenResult1 = MineField { cells = array ((0, 0), (2 - 1, 2 - 1)) [
        ((0, 0), Cell { cellState = Opened, isMine = False}),
        ((0, 1), Cell { cellState = Opened, isMine = False}),
        ((1, 0), Cell { cellState = Opened, isMine = False}),
        ((1, 1), Cell { cellState = Unopened, isMine = True})
    ]
}

-- __
-- _💥
expectedMineOpenResult1 :: MineField
expectedMineOpenResult1 = MineField { cells = array ((0, 0), (2 - 1, 2 - 1)) [
        ((0, 0), Cell { cellState = Opened, isMine = False}),
        ((0, 1), Cell { cellState = Opened, isMine = False}),
        ((1, 0), Cell { cellState = Opened, isMine = False}),
        ((1, 1), Cell { cellState = Opened, isMine = True})
    ]
}

-- 0F
-- 0X
smallField2 :: MineField
smallField2 = MineField { cells = array ((0, 0), (2 - 1, 2 - 1)) [
        ((0, 0), Cell { cellState = Unopened, isMine = False}),
        ((0, 1), Cell { cellState = Unopened, isMine = False}),
        ((1, 0), Cell { cellState = Flagged, isMine = False}),
        ((1, 1), Cell { cellState = Unopened, isMine = True})
    ]
}

-- _F
-- _X
expectedOpenResult2 :: MineField
expectedOpenResult2 = MineField { cells = array ((0, 0), (2 - 1, 2 - 1)) [
        ((0, 0), Cell { cellState = Opened, isMine = False}),
        ((0, 1), Cell { cellState = Opened, isMine = False}),
        ((1, 0), Cell { cellState = Flagged, isMine = False}),
        ((1, 1), Cell { cellState = Unopened, isMine = True})
    ]
}

-- _F
-- _💥
expectedMineOpenResult2 :: MineField
expectedMineOpenResult2 = MineField { cells = array ((0, 0), (2 - 1, 2 - 1)) [
        ((0, 0), Cell { cellState = Opened, isMine = False}),
        ((0, 1), Cell { cellState = Opened, isMine = False}),
        ((1, 0), Cell { cellState = Flagged, isMine = False}),
        ((1, 1), Cell { cellState = Opened, isMine = True})
    ]
}

-- X0
-- 0X
smallField3 :: MineField
smallField3 = MineField { cells = array ((0, 0), (2 - 1, 2 - 1)) [
        ((0, 0), Cell { cellState = Unopened, isMine = True}),
        ((0, 1), Cell { cellState = Unopened, isMine = False}),
        ((1, 0), Cell { cellState = Unopened, isMine = False}),
        ((1, 1), Cell { cellState = Unopened, isMine = True})
    ]
}

-- X0
-- _X
expectedOpenResult3 = MineField { cells = array ((0, 0), (2 - 1, 2 - 1)) [
        ((0, 0), Cell { cellState = Unopened, isMine = True}),
        ((0, 1), Cell { cellState = Opened, isMine = False}),
        ((1, 0), Cell { cellState = Unopened, isMine = False}),
        ((1, 1), Cell { cellState = Unopened, isMine = True})
    ]
}
