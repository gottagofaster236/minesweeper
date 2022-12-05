module Main (main) where
import Control.Monad.Random


exampleRand :: (RandomGen g) => Rand g Int
exampleRand = do
    ret <- getRandomR (100, 123)
    return ret


main :: IO ()
main = do
    g <- getStdGen
    let coolRandom = evalRand exampleRand g
    print $ "Hello, world, stuff is " ++ (show coolRandom)
