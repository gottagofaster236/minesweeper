module MineField(CellState(..), Cell(..), MineField(..), GameState(..),
    generateMineField, width, height, cellNumbers, openCell, flagCell, countMinesLeft, getGameState) where
import Data.Array
import Control.Monad.Random
import System.Random.Shuffle
import qualified Data.Set as Set
import Control.Monad.State

data CellState = Unopened | Opened | Flagged
    deriving (Show, Eq)

data Cell = Cell { cellState :: CellState, isMine :: Bool }
    deriving (Show, Eq)

newtype MineField = MineField { cells :: Array (Int, Int) Cell }
    deriving (Show, Eq)

data GameState = Running | Won | Lost
    deriving (Show, Eq)

-- Takes in the size (w, h) of the field and the number of mines.
generateMineField :: (RandomGen g) => (Int, Int) -> Int -> Rand g MineField
generateMineField (fieldWidth, fieldHeight) minesCount = do
    let cellsCount = fieldWidth * fieldHeight
    let isMineUnshuffled = replicate minesCount True ++ replicate (cellsCount - minesCount) False
    isMineShuffled <- shuffleM isMineUnshuffled
    let isMine2DArray = listArray ((0, 0), (fieldWidth - 1, fieldHeight - 1)) isMineShuffled
    let cells2DArray = fmap (\isCellMine -> Cell { cellState = Unopened, isMine = isCellMine }) isMine2DArray
    return MineField { cells = cells2DArray }

width :: MineField -> Int
width field = 1 + fst (snd $ bounds $ cells field)

height :: MineField -> Int
height field = 1 + snd (snd $ bounds $ cells field)

isIndexInRange :: MineField -> (Int, Int) -> Bool
isIndexInRange field = inRange (bounds (cells field))

cellNumbers :: MineField -> Array (Int, Int) Int
cellNumbers field =
        listArray (bounds (cells field)) [ neighboringMineCount position | position <- indices (cells field) ]
    where
        neighboringMineCount :: (Int, Int) -> Int
        neighboringMineCount (x, y) = sum $ map (fromEnum . safeIsMine)
            [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1), (x + 1, y + 1)]

        safeIsMine :: (Int, Int) -> Bool
        safeIsMine position | isIndexInRange field position = isMine $ cells field!position
        safeIsMine _ = False

openCell :: MineField -> (Int, Int) -> MineField
openCell field position | cellState (cells field!position) /= Unopened = field 
-- We opened a mine cell. Show the whole field.
openCell field position | isMine (cells field!position) = do
        field { cells = fmap openCellIfNotOpened (cells field) }
    where
        openCellIfNotOpened :: Cell -> Cell
        openCellIfNotOpened cell | cellState cell == Unopened = cell { cellState = Opened }
        openCellIfNotOpened cell = cell
-- We opened an ordinary cell.
openCell field position = do
        let fieldCells = cells field
        let positionsToOpen = Set.toList (execState (findPositionsToOpen position) Set.empty)
        let modifiedPositionsAndCells =
                map (\modifiedPosition -> 
                    (modifiedPosition, (cells field!modifiedPosition){ cellState = Opened }))
                positionsToOpen
        field { cells = fieldCells//modifiedPositionsAndCells }
    where
        findPositionsToOpen :: (Int, Int) -> State (Set.Set (Int, Int)) ()
        findPositionsToOpen currentPosition | not (isIndexInRange field currentPosition) = return ()
        findPositionsToOpen currentPosition | cellState (cells field!currentPosition) /= Unopened = return ()
        findPositionsToOpen (x, y) = do
            visitedPositions <- get
            if Set.member (x, y) visitedPositions then
                return ()
            else do
                put $ Set.insert (x, y) visitedPositions
                when (cellNumbers field!(x, y) == 0) $ do
                    findPositionsToOpen (x - 1, y)
                    findPositionsToOpen (x + 1, y)
                    findPositionsToOpen (x, y - 1)
                    findPositionsToOpen (x, y + 1)


flagCell :: MineField -> (Int, Int) -> MineField
flagCell field position = do
    let fieldCells = cells field
    let oldCell = fieldCells!position
    let newState = case cellState oldCell of
            Unopened -> Flagged
            Flagged -> Unopened
            Opened -> Opened
    let newCells = fieldCells//[(position, oldCell { cellState = newState })]
    field { cells = newCells }

countMinesLeft :: MineField -> Int
countMinesLeft field = do
    let countMines = sum $ fmap (fromEnum . isMine) (cells field)
    let countFlags = sum $ fmap (fromEnum . (\cell -> cellState cell == Flagged)) (cells field)
    countMines - countFlags

getGameState :: MineField -> GameState
getGameState field | hasOpenedMines = Lost
    where 
        hasOpenedMines = any (\cell -> cellState cell == Opened && isMine cell) (cells field)
getGameState field | hasUnopenedNonMines = Running
    where 
        hasUnopenedNonMines = any (\cell -> cellState cell == Unopened && (not . isMine) cell) (cells field)
getGameState _ = Won
