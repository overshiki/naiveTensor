

module NaiveTensor.Statistic (
        nt_max, nt_min
            ) where 

import NaiveTensor.NTensor 


nested_apply :: Ord a => (a -> a -> a) -> (NaiveTensor a) -> a
nested_apply func (Tensor ax@((Leaf x):xs)) = foldl func x (map get_content xs)
nested_apply func (Tensor ax@(x@(Tensor _x):xs)) = foldl func (nested_apply func x) (map (nested_apply func) xs)

nt_min :: Ord a => (NaiveTensor a) -> a 
nt_min = nested_apply min 

nt_max :: Ord a => (NaiveTensor a) -> a
nt_max = nested_apply max 


main :: IO ()
main = do
    let nnt = Tensor [(range 1 10), (range 11 20)]
    print $ nt_min nnt 
    print $ nt_max nnt