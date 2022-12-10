{-# LANGUAGE OverloadedStrings #-}

module MineField
    ( CellState(..)
    , Cell(..)
    , MineField(..)
    , GameState(..)
    , generateMineField
    , width
    , height
    , cellNumber
    , cellLabel
    , openCell
    , flagCell
    , countMinesLeft
    , getGameState
    , isPositionInRange
    ) where

import Control.Monad.Random
import Control.Monad.State
import Data.Aeson (ToJSON(toJSON), (.=))
import Data.Aeson.Types (object)
import Data.Array
import qualified Data.Set as Set
import System.Random.Shuffle

data CellState
    = Unopened
    | Opened
    | Flagged
    deriving (Show, Eq)

data Cell =
    Cell
        { cellState :: CellState
        , isMine :: Bool
        }
    deriving (Show, Eq)

newtype MineField =
    MineField
        { cells :: Array (Int, Int) Cell
        }
    deriving (Show, Eq)

data GameState
    = Running
    | Won
    | Lost
    deriving (Show, Eq)

-- Takes in the size (w, h) of the field and the number of mines.
generateMineField :: (RandomGen g) => (Int, Int) -> Int -> Rand g (Maybe MineField)

generateMineField (fieldWidth, fieldHeight) minesCount
    | fieldWidth <= 0 || fieldHeight <= 0 || fieldWidth > 50 || fieldHeight > 50
        || minesCount < 0 || minesCount > fieldWidth * fieldHeight = return Nothing
generateMineField (fieldWidth, fieldHeight) minesCount = do
    let isMineUnshuffled =
            replicate minesCount True ++
            replicate (fieldWidth * fieldHeight - minesCount) False
    isMineShuffled <- shuffleM isMineUnshuffled
    let isMine2DArray =
            listArray ((0, 0), (fieldWidth - 1, fieldHeight - 1)) isMineShuffled
    let cells2DArray =
            fmap
                (\isCellMine -> Cell {cellState = Unopened, isMine = isCellMine})
                isMine2DArray
    return $ Just $ MineField {cells = cells2DArray}

width :: MineField -> Int
width field = 1 + fst (snd $ bounds $ cells field)

height :: MineField -> Int
height field = 1 + snd (snd $ bounds $ cells field)

isPositionInRange :: MineField -> (Int, Int) -> Bool
isPositionInRange field = inRange (bounds (cells field))

cellNumber :: MineField -> (Int, Int) -> Int
cellNumber field (x, y) =
    sum $
    map (fromEnum . safeIsMine)
        [ (x - 1, y)
        , (x + 1, y)
        , (x, y - 1)
        , (x, y + 1)
        , (x - 1, y - 1)
        , (x - 1, y + 1)
        , (x + 1, y - 1)
        , (x + 1, y + 1)
        ]
  where
    safeIsMine :: (Int, Int) -> Bool
    safeIsMine position
        | isPositionInRange field position = isMine $ cells field ! position
    safeIsMine _ = False

cellLabel :: MineField -> (Int, Int) -> Char
cellLabel field position = do
    let cell = cells field ! position
    case cellState cell of
        Unopened -> 'U'
        Flagged -> 'F'
        Opened ->
            if isMine cell
                then 'M'
                else toEnum (cellNumber field position + fromEnum '0')

openCell :: MineField -> (Int, Int) -> MineField
openCell field position
    | cellState (cells field ! position) /= Unopened = field
-- We opened a mine cell. Show the whole field.
openCell field position
    | isMine (cells field ! position) = do
        field {cells = fmap openCellIfNotOpened (cells field)}
  where
    openCellIfNotOpened :: Cell -> Cell
    openCellIfNotOpened cell
        | cellState cell == Unopened = cell {cellState = Opened}
    openCellIfNotOpened cell = cell
-- We opened an ordinary cell.
openCell field position = do
    let fieldCells = cells field
    let positionsToOpen =
            Set.toList (execState (findPositionsToOpen position) Set.empty)
    let modifiedPositionsAndCells =
            map
                (\modifiedPosition ->
                     ( modifiedPosition
                     , (cells field ! modifiedPosition) {cellState = Opened}))
                positionsToOpen
    field {cells = fieldCells // modifiedPositionsAndCells}
  where
    findPositionsToOpen :: (Int, Int) -> State (Set.Set (Int, Int)) ()
    findPositionsToOpen currentPosition
        | not (isPositionInRange field currentPosition) = return ()
    findPositionsToOpen currentPosition
        | cellState (cells field ! currentPosition) /= Unopened = return ()
    findPositionsToOpen (x, y) = do
        visitedPositions <- get
        if Set.member (x, y) visitedPositions
            then return ()
        else do
            put $ Set.insert (x, y) visitedPositions
            when (cellNumber field (x, y) == 0) $ do
                findPositionsToOpen (x - 1, y)
                findPositionsToOpen (x + 1, y)
                findPositionsToOpen (x, y - 1)
                findPositionsToOpen (x, y + 1)

flagCell :: MineField -> (Int, Int) -> MineField
flagCell field position = do
    let fieldCells = cells field
    let oldCell = fieldCells ! position
    let newState =
            case cellState oldCell of
                Unopened -> Flagged
                Flagged -> Unopened
                Opened -> Opened
    let newCells = fieldCells // [(position, oldCell {cellState = newState})]
    field {cells = newCells}

countMinesLeft :: MineField -> Int
countMinesLeft field = do
    let countMines = sum $ fmap (fromEnum . isMine) (cells field)
    let countFlags =
            sum $
            fmap (fromEnum . (\cell -> cellState cell == Flagged)) (cells field)
    countMines - countFlags

getGameState :: MineField -> GameState
getGameState field
    | hasOpenedMines = Lost
  where
    hasOpenedMines =
        any (\cell -> cellState cell == Opened && isMine cell) (cells field)
getGameState field
    | hasUnopenedNonMines = Running
  where
    hasUnopenedNonMines =
        any
            (\cell -> cellState cell == Unopened && (not . isMine) cell)
            (cells field)
getGameState _ = Won

instance ToJSON MineField where
    toJSON field = do
        object
            [ "gameState" .= show (getGameState field)
            , "cellLabels" .=
              [ [cellLabel field (x, y) | y <- [0 .. height field - 1]]
              | x <- [0 .. width field - 1]
              ]
            , "countMinesLeft" .= countMinesLeft field
            ]
