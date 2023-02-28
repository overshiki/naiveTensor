import System.Process 
import Data.List.Split

main :: IO ()
main = do 
    r <- readProcess "ls" [] []
    let rs = splitOn "\n" r
    print rs