import System.Random
import Control.Monad (replicateM)

main = replicateM 10 (randomIO :: IO Float) >>= print