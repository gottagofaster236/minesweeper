{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status400, status404)
import Data.ByteString.Lazy.Internal (fromStrict, packChars)
import Data.ByteString.Internal (unpackChars)


-- | A simple example data type
data Person = Person
    { name :: String
    , age  :: Int
    }

examplePerson = Person { name = "Joe", age = 123 }

-- | Convert a 'Person' value to a JSON value
instance ToJSON Person where
    toJSON (Person name age) = object
      [ "name" .= name
      , "age"  .= age
      ]

-- | Convert a JSON value to a 'Person' value
instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "name"
        <*> v .: "age"

main :: IO ()
main = do
    putStrLn "Starting server on port 3000"
    run 3000 app

app :: Application
app request respond = do
    body <- requestBody request
    let person = decode (fromStrict body) :: Maybe Person
    case rawPathInfo request of
        "/hello" ->
            case person of
                Just p  -> respond $ responseLBS status200 [("Content-Type", "application/json")] (encode p)
                Nothing -> respond $ responseLBS status400 [("Content-Type", "text/plain")] (packChars ("Invalid JSON" ++ (unpackChars body)))
        _ ->
            -- respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not found"
            respond $ responseLBS status200 [("Content-Type", "application/json")] (encode examplePerson)
