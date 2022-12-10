{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Server
    ( app
    ) where

import Control.Concurrent.STM
import Control.Monad.Random
import Control.Monad.Reader
import Data.Default.Class
import qualified Data.Map as Map
import Data.String
import Data.Text.Lazy (Text)
import MineField
import Network.HTTP.Types
import Network.Wai
import Prelude ()
import Prelude.Compat
import Web.Scotty.Trans

-- Global state management is based on this example: https://github.com/scotty-web/scotty/blob/master/examples/globalstate.hs
newtype AppState =
    AppState
        { games :: Map.Map String MineField
        }

instance Default AppState where
    def = AppState Map.empty

newtype AppStateM a =
    AppStateM
        { runAppStateM :: ReaderT (TVar AppState) IO a
        }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

appStateM :: MonadTrans t => AppStateM a -> t AppStateM a
appStateM = lift

getState :: (AppState -> b) -> AppStateM b
getState f = ask >>= liftIO . readTVarIO >>= return . f

modifyState :: (AppState -> AppState) -> AppStateM ()
modifyState f = ask >>= liftIO . atomically . flip modifyTVar' f

app :: IO Application
app = do
    appState <- newTVarIO def
    let runActionToIO m = runReaderT (runAppStateM m) appState
    scottyAppT runActionToIO appWithoutState

appWithoutState :: ScottyT Text AppStateM ()
appWithoutState = do
    defaultHandler $ \errorText -> do
        status status400
        text errorText
    get "/" $ file "./static/game.html"
    get "/game/:gameId" $ file "./static/game.html"
    post "/newGame" $ do
        fieldWidth <- param "fieldWidth"
        fieldHeight <- param "fieldHeight"
        minesCount <- param "minesCount"
        newGameRequest fieldWidth fieldHeight minesCount
    get "/game/:gameId/field" $ do
        gameId <- param "gameId"
        field <- fieldByGameId gameId
        json field
    post "/game/:gameId/openCell" (cellActionRequest openCell)
    post "/game/:gameId/flagCell" (cellActionRequest flagCell)

newGameRequest :: Int -> Int -> Int -> ActionT Text AppStateM ()
newGameRequest fieldWidth fieldHeight minesCount = do
    gameId <- liftIO $ evalRandIO randomGameId
    mineFieldMaybe <-
        liftIO $
        evalRandIO $ generateMineField (fieldWidth, fieldHeight) minesCount
    case mineFieldMaybe of
        Just mineField -> do
            appStateM $
                modifyState $ \state -> do
                    let newGames = Map.insert gameId mineField (games state)
                    state {games = newGames}
            json $ Map.singleton ("gameId" :: String) gameId
        Nothing -> raise $ stringError "Invalid width, height, or mine count"

randomGameId :: (RandomGen g) => Rand g String
randomGameId = do
    replicateM 32 randomChar
  where
    randomChar :: (RandomGen g) => Rand g Char
    randomChar = do
        let possibleChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
        charIndex <- getRandomR (0, length possibleChars - 1)
        return $ possibleChars !! charIndex

fieldByGameId :: String -> ActionT Text AppStateM MineField
fieldByGameId gameId = do
    storedGames <- appStateM $ getState games
    case Map.lookup gameId storedGames of
        Nothing -> raise $ stringError "Invalid gameId"
        Just field -> return field

cellActionRequest ::
       (MineField -> (Int, Int) -> MineField) -> ActionT Text AppStateM ()
cellActionRequest action = do
    x <- param "x"
    y <- param "y"
    gameId <- param "gameId"
    field <- fieldByGameId gameId
    if not (isPositionInRange field (x, y))
        then raise $ stringError "Invalid x or y"
    else if getGameState field /= Running
        then raise $ stringError "Game is finished already"
    else do
        let newField = action field (x, y)
        appStateM $
            modifyState $ \state -> do
                let newGames = Map.insert gameId newField (games state)
                state {games = newGames}
