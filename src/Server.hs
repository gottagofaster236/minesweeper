{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Server
    ( app
    , runWebM
    ) where

import MineField
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Default.Class
import Data.String
import Data.Text.Lazy (Text)
import Prelude ()
import Prelude.Compat
import Web.Scotty.Trans
import qualified Data.Map as Map
import Control.Monad.Random
import Network.HTTP.Types

-- The server code is based on this example: https://github.com/scotty-web/scotty/blob/master/examples/globalstate.hs

newtype AppState = AppState { games :: Map.Map String MineField }

instance Default AppState where
    def = AppState Map.empty

newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

-- Scotty's monads are layered on top of our custom monad.
-- We define this synonym for lift in order to be explicit
-- about when we are operating at the 'WebM' layer.
webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- Some helpers to make this feel more like a state monad.
gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

app :: ScottyT Text WebM ()
app = do
    defaultHandler $ \s -> do
        status status400
        text s

    post "/newGame" $ do
        fieldWidth :: Int <- param "fieldWidth"
        fieldHeight :: Int <- param "fieldHeight"
        minesCount :: Int <- param "minesCount"
        newGameRequest fieldWidth fieldHeight minesCount
    
    get "/game/:gameId" $ do
        gameId <- param "gameId"
        game <- gameById gameId
        json game

newGameRequest :: Int -> Int -> Int -> ActionT Text WebM ()
newGameRequest fieldWidth fieldHeight minesCount = do
    gameId <- liftIO $ evalRandIO randomGameId
    mineFieldMaybe <- liftIO $ evalRandIO $ generateMineField (fieldWidth, fieldHeight) minesCount
    case mineFieldMaybe of
        Just mineField -> do
            webM $ modify $ \st -> do
                st { games = Map.insert gameId mineField (games st) }
            json $ Map.singleton ("gameId" :: String) gameId
        Nothing -> raise $ stringError "Invalid width, height, or mine count"

randomGameId :: (RandomGen g) => Rand g String
randomGameId = do
    replicateM 32 randomChar
  where
    randomChar :: (RandomGen g) => Rand g Char
    randomChar = do
        let possibleChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
        charIndex <- getRandomR (0, length possibleChars - 1)
        return $ possibleChars !! charIndex

gameById :: String -> ActionT Text WebM MineField
gameById gameId = do
    storedGames <- webM $ gets games
    case Map.lookup gameId storedGames of
        Nothing -> raise $ stringError "Invalid gameId"
        Just game -> return game
