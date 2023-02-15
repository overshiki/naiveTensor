{-# LANGUAGE ScopedTypeVariables #-}
import NTensor 
-- import NTensor (constructor Tensor)
-- import NTensor (constructor Leaf)
-- import NTensor (constructor Null)
import System.Random
import Control.Monad (replicateM)

ioconcat :: IO (NaiveTensor Float) -> IO (NaiveTensor Float) -> IO (NaiveTensor Float)
ioconcat x y = x >>= \x -> y >>= \y -> return (x <> y)

-- random constructor 
flattenRandom :: Int -> IO (NaiveTensor Float)
-- flattenRandom :: Int -> IO ([Float])
flattenRandom x = do 
    x <- replicateM x (randomIO :: IO Float)
    return (Tensor (map Leaf x))
    -- return x



-- ioreduce :: IO [Float] -> IO Float -> IO [Float]
-- ioreduce (IO xs) (IO x) = do 
--     return xs ++ [x]

-- ioconcat :: [IO Float] -> IO [Float]
-- ioconcat [IO x] = do
--     return [x]
-- ioconcat x:xs = do 



-- randn :: [Int] -> IO (NaiveTensor Float)
-- randn [x] = flattenRandom x 
-- randn (x:xs) = rand_repeat x (randn xs)
--         where rand_repeat :: Int -> IO (NaiveTensor Float) -> IO (NaiveTensor Float)
--             rand_repeat x (IO io_ntensor) = iocat $ map (randn (size io_ntensor)) [1..x]
--             where iocat :: [IO (NaiveTensor Float)] -> IO (NaiveTensor Float)
--                 iocat [IO x] = 



main :: IO () 
main = do 
    print $ flattenOne 10
    x <- flattenRandom 10
    print x