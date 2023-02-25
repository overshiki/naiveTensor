module NaiveTensor.Transpose (transpose, 
                            transposeAt,
                            transposeFor
                ) where
import NaiveTensor.NTensor 

-- for transpose 

-- take i'th element in each element of a list of NaiveTensor, wrap them into a new NaiveTensor
ttake :: [NaiveTensor a] -> Int -> NaiveTensor a 
ttake xa i = Tensor $ map (_ttake i) xa 
    where _ttake i x@(Tensor xxa) = xxa !! i
            -- TODO: FIX THIS
          _ttake i x@(Leaf a) = x

transpose :: NaiveTensor a -> NaiveTensor a 
transpose (Tensor xa@(x:xs)) = 
        Tensor $ map (ttake xa) [0..inner_len-1] 
    where 
        inner_len = length $ unwrap2list $ x

transposeAt :: Int -> NaiveTensor a -> NaiveTensor a 
transposeAt s x@(Tensor xs)
        | s==0 = transpose x 
        | otherwise = Tensor (map (transposeAt $ s-1) xs)


-- for example: when input [2,1,0], we want to specify in which order we should do transposeAt, the answer is [[0, 1], [1, 2], [0, 1]], so this function should return [0, 1, 0]
-- the strategy is that we find the largest indices in the output, for example, in [2,1,0], we firstly find 0 at the position of 2. So that we want to pop it to the position 2, which should follow the path of [[0, 1], [1, 2]]. Doing so, the order of other indices does not change 
-- then we pop the second largest on, by calling the function recursively
transpose_schedule :: [Int] -> [Int]
transpose_schedule x = _transpose_schedule (reverse x) ((length x)-2) 
        where 
            _transpose_schedule ax@(x:xs) _len = [x.._len] ++ (_transpose_schedule (shift x xs) (_len-1))
            _transpose_schedule [] _ = []
            shift x xs = map (_shift x) xs 
                where 
                    _shift x y
                        | x < y     = y-1
                        | otherwise = y 

transposeFor :: [Int] -> NaiveTensor a -> NaiveTensor a
transposeFor schedule x = _transposeFor (transpose_schedule schedule) x 
        where 
            _transposeFor (i:is) x = _transposeFor is (transposeAt i x)
            _transposeFor [] x = x


main :: IO ()
main = do 
    x <- return $ (wraplift $ ones [3]) <> (wraplift $ zeros [3])
    print x 

    y <- return $ transpose x 
    print y    

    y <- return . transpose $ wraplift x 
    print y    
    y <- return $ transposeAt 1 (wraplift x) 
    print y 
    y <- return $ transposeAt 2 (wraplift x) 
    print y 

    print (transpose_schedule [2,1,0])
    print $ wraplift x
    print (transposeFor [0,2,1] (wraplift x))
    print $ ndim (transposeFor [0,2,1] (wraplift x))