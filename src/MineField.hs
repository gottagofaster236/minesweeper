module MineField(CellState(..), Cell(..), MineField(..), GameState(..),
    width, height, generateMineField, openCell, flagCell, countMinesLeft) where
import GHC.Arr
import Control.Monad.Random
import System.Random.Shuffle
import qualified Data.Set as Set

data CellState = Unopened | Opened | Flagged
    deriving (Show, Eq)

data Cell = Cell { cellState :: CellState, isMine :: Bool }
    deriving (Show, Eq)

newtype MineField = MineField { cells :: Array (Int, Int) Cell }
    deriving (Show, Eq)

data GameState = Running | Won | Lost
    deriving (Show, Eq)

width :: MineField -> Int
width field = 1 + fst (snd $ bounds $ cells field)

height :: MineField -> Int
height field = 1 + snd (snd $ bounds $ cells field)

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
    return MineField { cells = cells2DArray }

openCell :: MineField -> (Int, Int) -> MineField
openCell field position | cellState (cells field!position) /= Unopened = field 
-- We opened a mine cell. Show the whole field.
openCell field position | isMine (cells field!position) = do
        field {cells = amap openCellIfNotOpened (cells field) }
    where
        openCellIfNotOpened :: Cell -> Cell
        openCellIfNotOpened cell | cellState cell == Unopened = cell { cellState = Opened }
        openCellIfNotOpened cell = cell
-- We opened an ordinary cell.
openCell field position = do
        let fieldCells = cells field
        let positionsToOpen = Set.toList $ findPositionsToOpen position Set.empty
        let modifiedPositionsAndCells =
             map (\modifiedPosition -> 
                (modifiedPosition, (cells field!modifiedPosition){ cellState = Opened }))
             positionsToOpen
        field { cells = fieldCells//modifiedPositionsAndCells }
    where
        -- Takes in the field, the current position, and the accumulator.
        findPositionsToOpen :: (Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
        findPositionsToOpen (x, y) acc | x < 0 || x >= width field || y < 0 || y >= height field = acc
        findPositionsToOpen currentPosition acc | cellState (cells field!currentPosition) /= Unopened = acc
        findPositionsToOpen currentPosition acc | isMine (cells field!currentPosition) = acc
        findPositionsToOpen currentPosition acc | Set.member currentPosition acc = acc
        findPositionsToOpen (x, y) acc = do
            let acc1 = Set.insert (x, y) acc
            let acc2 = findPositionsToOpen (x - 1, y) acc1
            let acc3 = findPositionsToOpen (x + 1, y) acc2
            let acc4 = findPositionsToOpen (x, y - 1) acc3
            let acc5 = findPositionsToOpen (x, y + 1) acc4
            acc5


flagCell :: MineField -> (Int, Int) -> MineField
flagCell field position = do
    let fieldCells = cells field
    let oldCell = fieldCells!position
    let newCells = if cellState oldCell == Unopened then
            fieldCells//[(position, oldCell { cellState = Flagged })]
        else
            fieldCells
    field { cells = newCells }

countMinesLeft :: MineField -> Int
countMinesLeft _ = 0

getGameState :: MineField -> GameState
getGameState _ = Running
