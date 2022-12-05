module MineField(CellState(..), Cell(..), MineField(..), GameState(..), MineGame(..),
    generateMineField, openField, markField, countMinesLeft) where
import GHC.Arr
import Control.Monad.Random
import System.Random.Shuffle

data CellState = Unopened | Opened | Flagged
    deriving (Eq)

data Cell = Cell { cellState :: CellState, isMine :: Bool }
    deriving (Eq)

data MineField = MineField { width :: Int, height :: Int, cells :: Array (Int, Int) Cell }
    deriving (Eq)

data GameState = NotStarted | Running | Won | Lost
    deriving (Eq)

data MineGame = MineGame MineField GameState
    deriving (Eq)

-- Takes in the size (w, h) of the field and the number of mines.
generateMineField :: (RandomGen g) => (Int, Int) -> Int -> Rand g MineField
generateMineField (fieldWidth, fieldHeight) minesCount = do
    let cellsCount = fieldWidth * fieldHeight
    let isMineUnshuffled = replicate minesCount True ++ replicate (cellsCount - minesCount) False
    isMineShuffled <- shuffleM isMineUnshuffled
    let isMine2DList = [((i, j), isMineShuffled!!(i + j * fieldWidth)) | i <- [0..fieldWidth - 1],
                                                                         j <- [0..fieldHeight - 1]]
    let isMine2DArray = array ((0, 0), (fieldWidth - 1, fieldHeight - 1)) isMine2DList
    let cells2DArray = amap (\isCellMine -> Cell { cellState = Unopened, isMine = isCellMine }) isMine2DArray
    return MineField { width = fieldWidth, height = fieldHeight, cells = cells2DArray }

openField :: MineField -> (Int, Int) -> MineField
openField field _ = field

markField :: MineField -> (Int, Int) -> MineField
markField field _ = field

countMinesLeft :: MineField -> Int
countMinesLeft _ = 0
