module NaiveTensor.Reduce (reduceOnce) where 

import NaiveTensor.NTensor 

reduceOnce :: ([a] -> a) -> (NaiveTensor a) -> (NaiveTensor a)
reduceOnce reduceFunc (Tensor ax@((Leaf x):xs)) = Leaf (reduceFunc (map get_content ax))
reduceOnce reduceFunc (Tensor ax@((Tensor x):xs)) = Tensor (map (reduceOnce reduceFunc) ax)


main :: IO ()
main = do 
    let nnt = Tensor [(range 1 10), (range 11 20)]
    print $ reduceOnce sum nnt
    let nnt = ones [2,2,2]
    print $ reduceOnce sum nnt