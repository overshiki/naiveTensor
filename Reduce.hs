module NaiveTensor.Reduce (reduceAll, 
                            reduceOnce, 
                            reduceHead,
                            reduceEnd,
                            sumproduct) where 

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

reduceEnd :: (a -> a -> a) -> (NaiveTensor a) -> (NaiveTensor a)
reduceEnd reduceFunc 
            ta@(Tensor ax@((Leaf x):xs)) 
            = Leaf (reduceAll reduceFunc ta)
reduceEnd reduceFunc 
            ta@(Tensor ax@((Tensor x):xs)) 
            = Tensor (map (reduceEnd reduceFunc) ax)

productEnd :: (Num a) => NaiveTensor a 
                        -> NaiveTensor a 
                        -> NaiveTensor a 
productEnd (Tensor ax@((Tensor x):xs)) y 
           = Tensor (map (\xx->productEnd xx y) ax)

productEnd nta@(Tensor ax@((Leaf x):xs)) 
           (Tensor ay@((Tensor y):ys)) 
           = Tensor (map (productEnd nta) ay)

productEnd nta@(Tensor ax@((Leaf x):xs)) 
           ntb@(Tensor ay@((Leaf y):ys)) 
           = Tensor (map func (zip ax ay))
                where 
                    func ((Leaf x), (Leaf y)) = Leaf (x*y)


-- do sumproduct at the last dimension 
-- for example: (x0, x1) @ (y0, y1) -> (x0, y0)
            -- (x0, x1, x2) @ (y0, y2) -> (x0, x1, y0)
sumproduct :: (Num a) => NaiveTensor a 
                        -> NaiveTensor a 
                        -> NaiveTensor a 
sumproduct x y = (reduceEnd (+)) $ productEnd x y


main :: IO ()
main = do 
    let nnt = Tensor [(range 1 10), (range 11 20)]
    print $ reduceOnce (+) nnt
    let nnt = ones [2,2,2]
    print $ reduceOnce (+) nnt
    let nnt = Tensor [(range 1 10), (range 11 20)]
    let nnnt = Tensor (take 3 (repeat nnt))
    print nnnt
    print "reduceHead:"
    print $ reduceHead (+) nnnt
    print "reduceEnd:"
    print $ reduceEnd (+) nnnt

    let a = fromList [2, 3] [1..6]
        b = fromList [2, 3] [2..7]
    print $ sumproduct a b
    