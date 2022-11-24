module Model.MineField (CellState, Cell, MineField, exampleRand) where
import GHC.Arr (Array)
import Control.Monad.Random

data CellState = Unopened | Opened | Flagged

data Cell = Cell { cellState :: CellState, isOpened :: Bool }

data MineField = MineField { width :: Int, height :: Int, cells :: Array (Int, Int) Cell }

--generateMineField :: (RandomGen g) => (Int, Int) -> MineField = do
--    getRandomR (0.0, 1.0)

exampleRand :: (RandomGen g) => Rand g Int
exampleRand = do
    ret <- getRandomR (100, 123)
    return ret
