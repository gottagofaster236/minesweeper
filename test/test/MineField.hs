{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wmissing-export-lists #-}
module Test.MineField where

import MineField
import Test.Tasty.HUnit
import Control.Monad.Random
import GHC.Arr

gen :: StdGen
gen = mkStdGen 0

unit_generateMineFieldHasCorrectMinesCount = do
        let mineField = evalRand (generateMineField (50, 100) 30) gen
        let mineCells = cells mineField
        ((0, 0), (49, 99)) @=? bounds mineCells
        50 @=? width mineField
        100 @=? height mineField
        let countMines = sum [boolToInt $ isMine $ (cells mineField)!(i, j) | i <- [0..49], j <- [0..99]]
        30 @=? countMines
    where
        boolToInt :: Bool -> Int
        boolToInt b = if b then 1 else 0

unit_generateMineFieldHasUnopenedFields = do
    let mineField = evalRand (generateMineField (123, 321) 7) gen
    let allFieldsUnopened = all (\cell -> cellState cell == Unopened) (elems $ cells $ mineField)
    assertBool "Fields should be unopened in the beginning" allFieldsUnopened

unit_generateMineFieldHasDifferentResults = do
        assertBool "Random fields should be different" (evalRand compareTwoBigFields gen)
    where
        compareTwoBigFields :: (RandomGen g) => Rand g Bool
        compareTwoBigFields = do
            mineField1 <- generateMineField (500, 500) 500
            mineField2 <- generateMineField (500, 500) 500
            return $ mineField1 /= mineField2