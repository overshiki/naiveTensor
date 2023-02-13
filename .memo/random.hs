import System.Random
import Control.Monad (replicateM)

-- main = replicateM 10 (randomIO :: IO Float) >>= print
main :: IO ()
main = do 
    x <- replicateM 10 (randomIO :: IO Float)
    print x