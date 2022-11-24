module Main (main) where
import Model.MineField
import Control.Monad.Random


main :: IO ()
main = do
    g <- getStdGen
    let coolRandom = evalRand exampleRand g
    print $ "Hello, world, stuff is " ++ (show coolRandom)
