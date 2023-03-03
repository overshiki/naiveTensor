module NaiveTensor.Reduce (reduceAll, reduceOnce, reduceHead) where 

import NaiveTensor.NTensor 


reduceAll :: (a -> a -> a) -> (NaiveTensor a) -> a
reduceAll func (Tensor ax@((Leaf x):xs)) = foldl func x (map get_content xs)
reduceAll func (Tensor ax@(x@(Tensor _x):xs)) = foldl func (reduceAll func x) (map (reduceAll func) xs)

foldReduce :: (a -> a -> a) -> [a] -> a 
foldReduce func (x:xs) = foldl func x xs 

reduceOnce :: (a -> a -> a) -> (NaiveTensor a) -> (NaiveTensor a)
reduceOnce reduceFunc (Tensor ax@((Leaf x):xs)) = Leaf (foldReduce reduceFunc (map get_content ax))
reduceOnce reduceFunc (Tensor ax@((Tensor x):xs)) = Tensor (map (reduceOnce reduceFunc) ax)

reduceHead :: (a -> a -> a) -> (NaiveTensor a) -> (NaiveTensor a)
reduceHead reduceFunc ta@(Tensor ax@((Leaf x):xs)) = ta 
reduceHead reduceFunc (Tensor ax@((Tensor x):xs)) = Tensor (map (Leaf . (reduceAll reduceFunc)) ax)


-- reduceOnce :: ([a] -> a) -> (NaiveTensor a) -> (NaiveTensor a)
-- reduceOnce reduceFunc (Tensor ax@((Leaf x):xs)) = Leaf (reduceFunc (map get_content ax))
-- reduceOnce reduceFunc (Tensor ax@((Tensor x):xs)) = Tensor (map (reduceOnce reduceFunc) ax)

-- reduceAll :: ([a] -> a) -> (NaiveTensor a) -> (NaiveTensor a)
-- reduceAll reduceFunc 

-- reduceHead :: ([a] -> a) -> (NaiveTensor a) -> (NaiveTensor a) 
-- reduceHead reduceFunc 


main :: IO ()
main = do 
    let nnt = Tensor [(range 1 10), (range 11 20)]
    print $ reduceOnce (+) nnt
    let nnt = ones [2,2,2]
    print $ reduceOnce (+) nnt
    print $ reduceHead (+) nnt