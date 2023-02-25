-- {-# LANGUAGE ScopedTypeVariables #-}
module NaiveTensor.NTRand (flattenRandom,
                            randn
                    ) where
import NaiveTensor.NTensor 
import System.Random
import Control.Monad (replicateM)

-- random constructor 
flattenRandom :: Int -> IO (NaiveTensor Float)
flattenRandom x = do 
    x <- replicateM x (randomIO :: IO Float)
    return (Tensor (map Leaf x))

ioconcat :: IO (NaiveTensor Float) -> IO (NaiveTensor Float) -> IO (NaiveTensor Float)
ioconcat x y = x >>= \x -> y >>= \y -> return (x <> y)

iolift :: IO (NaiveTensor Float) -> IO (NaiveTensor Float)
iolift x = x >>= \x -> return (wraplift x)
            
ioreduce :: [IO (NaiveTensor Float)] -> IO (NaiveTensor Float)
ioreduce (x:xs) = ioconcat (iolift x) (ioreduce xs)
ioreduce [] = return Null

randn :: [Int] -> IO (NaiveTensor Float)
randn [x] = flattenRandom x 
randn (x:xs) = ioreduce $ map (\i->(randn xs)) [1..x]


main :: IO () 
main = do 
    print $ flattenOne 10
    x <- flattenRandom 10
    print x

    x <- iolift $ flattenRandom 10
    print x
    x <- ioreduce (map (\i -> flattenRandom 3) [1..3])
    print x

    x <- randn [2,2,2]
    print x