module Main (main) where

import Server (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    putStrLn "Starting server on port 3000"
    app >>= run 3000
