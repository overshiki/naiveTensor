{-# LANGUAGE ScopedTypeVariables #-}
import NTensor 
import Data.List.Split

csv_parse :: String -> NaiveTensor Float
csv_parse x = Tensor $ map readline (lines x)
    where 
        readline line = Tensor $ map (\x->Leaf ((read x)::Float)) (splitOn "," line)

read_csv :: String -> IO (NaiveTensor Float)
read_csv csv_path = do 
    contents <- readFile csv_path
    return $ csv_parse contents


file_parse :: String -> String
file_parse contents = foldl (++) "" (splitOn "\n" contents)


-- read_file :: String -> IO (NaiveTensor Float)


main :: IO ()
main = do 
    x <- read_csv ".memo/d2tensor.csv"  
    print x

    x <- readFile "tensor.file"
    print . file_parse $ x