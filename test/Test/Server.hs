{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Server where

import Server
import Network.Wai
import Network.Wai.Test
import qualified Test.Tasty.HUnit as T
import Aeson.Match.QQ (Matcher, match, qq)
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Aeson as Aeson
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Text.Printf (printf)
import qualified Data.Text as Text
import Network.HTTP.Types
import qualified Data.HashMap.Strict as HashMap

serverTest :: Session () -> IO ()
serverTest session = do
    appInstance <- app
    runSession session appInstance

get :: BS.ByteString -> Session SResponse
get url = request $ setPath defaultRequest {requestMethod = methodGet} url

post :: BS.ByteString -> Session SResponse
post url = request $ setPath defaultRequest {requestMethod = methodPost} url

(@=?) :: T.HasCallStack => Eq a => Show a => a -> a -> Session ()
expected @=? actual = liftIO $ expected T.@=? actual

assertBool :: T.HasCallStack => String -> Bool -> Session ()
assertBool message condition = liftIO $ T.assertBool message condition

assertFailure :: T.HasCallStack => String -> Session a
assertFailure = liftIO . T.assertFailure

assertIsJust :: T.HasCallStack => Maybe a -> Session a
assertIsJust Nothing = assertFailure "Value should not be Nothing"
assertIsJust (Just a) = return a

assertIsRight :: T.HasCallStack => Either a b -> Session b
assertIsRight (Left _) = assertFailure "Value should not be Left"
assertIsRight (Right b) = return b

json :: T.HasCallStack => SResponse -> Session Aeson.Value
json response = do
    assertIsJust $ Aeson.decode $ simpleBody response

unit_webpage = serverTest $ do
    responseIndex <- get "/"
    assertStatus 200 responseIndex
    assertBodyContains "Minesweeper" responseIndex
    
    gameId <- newGameId (5, 5) 5
    responseGame <- get $ BS.pack $ "/game/" ++ gameId
    assertStatus 200 responseGame
    assertBodyContains "Minesweeper" responseGame

unit_newGame = serverTest $ do
    gameId <- newGameId (5, 5) 5
    32 @=? length gameId
    gameId2 <- newGameId (5, 5) 5
    assertBool "Game ids should be different" $ gameId /= gameId2

unit_newGameErrors = serverTest $ do
    responseNoParams <- post "/newGame"
    assertStatus 400 responseNoParams
    assertBody "Param: fieldWidth not found!" responseNoParams

    responseTooManyMines <-
        post "/newGame?fieldWidth=5&fieldHeight=5&minesCount=100&"
    assertStatus 400 responseTooManyMines
    assertBody "Invalid mines count" responseTooManyMines

newGameId :: T.HasCallStack => (Int, Int) -> Int -> Session String
newGameId (fieldWidth, fieldHeight) minesCount = do
    let url = printf "/newGame?fieldWidth=%d&fieldHeight=%d&minesCount=%d"
                  fieldWidth fieldHeight minesCount
    response <- post (BS.pack url)
    assertStatus 200 response
    responseJson <- json response
    let gameIdMatcher = [qq| {"gameId": _gameId} |]
    matched <- assertIsRight $ match gameIdMatcher responseJson
    (Aeson.String gameId) <- assertIsJust $ HashMap.lookup "gameId" matched
    return $ Text.unpack gameId

unit_field = serverTest $ do
    gameId <- newGameId (1, 1) 0
    response <- get $ BS.pack $ "/game/" ++ gameId ++ "/field"
    assertStatus 200 response
    responseJson <- json response
    let expected = [aesonQQ| {"gameState": "Running", "cellLabels": ["U"], "countMinesLeft": 0} |]
    expected @=? responseJson

unit_fieldErrors = serverTest $ do
    response <- get "game/invalidGameId/field"
    assertStatus 400 response
    assertBody "Invalid gameId" response

assertFieldMatches :: T.HasCallStack => Matcher Aeson.Value -> String -> Session ()
assertFieldMatches matcher gameId = do
    response <- get $ BS.pack $ "/game/" ++ gameId ++ "/field"
    assertStatus 200 response
    responseJson <- json response
    _ <- assertIsRight $ match matcher responseJson
    return ()

unit_openCell = serverTest $ do
    gameId <- newGameId (1, 1) 0
    assertFieldMatches [qq| {"cellLabels": ["U"], ...} |] gameId
    responseOpenCell <- post $ BS.pack $ "/game/" ++ gameId ++ "/openCell?x=0&y=0"
    assertStatus 200 responseOpenCell
    assertFieldMatches [qq| {"cellLabels": ["0"], ...} |] gameId

unit_openCellErrors = testCellActionErrors "openCell"

unit_flagCell = serverTest $ do
    gameId <- newGameId (1, 1) 0
    assertFieldMatches [qq| {"cellLabels": ["U"], ...} |] gameId
    responseOpenCell <- post $ BS.pack $ "/game/" ++ gameId ++ "/flagCell?x=0&y=0"
    assertStatus 200 responseOpenCell
    assertFieldMatches [qq| {"cellLabels": ["F"], ...} |] gameId

unit_flagCellErrors = testCellActionErrors "flagCell"

testCellActionErrors action = serverTest $ do
    responseInvalidGameId <- post $ BS.pack $ "/game/invalidGameId/" ++ action ++ "?x=0&y=0"
    assertStatus 400 responseInvalidGameId
    assertBody "Invalid gameId" responseInvalidGameId

    gameId <- newGameId (1, 1) 0
    let gamePrefix = "/game/" ++ gameId ++ "/"

    responseOutOfBounds <- post $ BS.pack $ gamePrefix ++ action ++ "?x=3&y=2"
    assertStatus 400 responseOutOfBounds
    assertBody "Invalid x or y" responseOutOfBounds
    
    responseFinalMove <- post $ BS.pack $ gamePrefix ++ "openCell?x=0&y=0"
    assertStatus 200 responseFinalMove
    responseGameFinished <- post $ BS.pack $ gamePrefix ++ action ++ "?x=0&y=0"
    assertStatus 400 responseGameFinished
    assertBody "Game is finished already" responseGameFinished
