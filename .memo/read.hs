import System.IO  
  
main = do  
    contents <- readFile "d2tensor.csv"  
    putStr contents