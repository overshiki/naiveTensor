module NaiveTensor.Order (
            find_inBetween_index,
            accumulate
            ) where 
import NaiveTensor.NTensor

data Order = DFS | BFS
-- data IndexInBetween = Index Int | Below | Above deriving (Eq, Show)
data OneHot = Hot | Cold deriving (Eq, Show)

union :: [OneHot] -> OneHot
union (Hot:xs) = Hot 
union (Cold:xs) = union xs 
union [] = Cold


_onehot_inBetween :: Ord a => a -> [a] -> OneHot -> [OneHot]
_onehot_inBetween t (x:xs) h
            | t >= x = Cold:(_onehot_inBetween t xs h)
            | t < x = h:(_onehot_inBetween t xs Cold)
_onehot_inBetween t [] h = []

onehot_inBetween :: Ord a => a -> [a] -> [OneHot]
onehot_inBetween t (x:xs)
            | t >= x = Cold:(_onehot_inBetween t xs Hot)
            | t < x = Hot:(_onehot_inBetween t xs Cold) 

onehot_inBetween t [] = []

onehot_index :: [OneHot] -> Int 
onehot_index (Hot:xs) = 0
onehot_index (Cold:x:xs) = 1 + (onehot_index (x:xs))
onehot_index [Cold] = 0

find_inBetween :: Ord a => a -> (NaiveTensor a) -> [[OneHot]]
find_inBetween i (Tensor ax@(x@(Leaf _x):xs)) = (onehot_inBetween i (map get_content ax)):[]

find_inBetween i (Tensor ax@(x@(Tensor _x):xs)) = nonehot:(find_inBetween i next_a)
        where 
            _reduce (ind:indices) = foldl (++) ind indices
            -- (ind:indices) = map ((map union) . (find_inBetween i)) ax 
            -- nonehot = foldl (++) ind indices
            nonehot = _reduce $ map ((map union) . (find_inBetween i)) ax 
            index = onehot_index nonehot
            next_a = ax !! index

find_inBetween_index :: Ord a => a -> (NaiveTensor a) -> [Int]
find_inBetween_index i xs = xx 
        where 
            onehot = find_inBetween i xs 
            xx = map onehot_index onehot




_end :: [a] -> a 
_end = head . reverse

end :: NaiveTensor a -> a 
end (Tensor ax@((Leaf x):xs)) = get_content $ _end ax
end (Tensor ax@(x1@(Tensor x):xs)) = end $ _end ax


accumulate :: Num a => (NaiveTensor a) -> (NaiveTensor a)
accumulate (Tensor ax@(x1@(Leaf x):x2:xs)) = Tensor (_accumulate ax)
            where 
                _accumulate (x1@(Leaf x):x2:xs) = x1:(_accumulate $ (add x1 x2):xs)
                _accumulate [x] = [x]
                add (Leaf x1) (Leaf x2) = Leaf (x1+x2)


accumulate (Tensor ax@(x1@(Tensor x):x2:xs)) = Tensor (_accumulate 0 ax)
            where 
                _accumulate offset (x1@(Tensor x):x2:xs) = ax1:(_accumulate acc_offset (x2:xs))
                            where 
                                ax1 = fmap (+offset) (accumulate x1)
                                acc_offset = end ax1
                _accumulate offset [x] = [fmap (+offset) (accumulate x)]


main :: IO ()
main = do 
    let xs = [1,2,3,4,5,6]
    print 0
    print $ onehot_inBetween 0 xs
    print 4
    print $ onehot_inBetween 4 xs
    print 7
    print $ onehot_inBetween 7 xs
    print 8
    print $ onehot_inBetween 8 xs

    let xs = [0.1, 0.25, 0.45, 0.8, 1.0]
    print 0.05
    print $ onehot_inBetween 0.05 xs
    print 0.9
    print $ onehot_inBetween 0.9 xs


    print . onehot_index $ onehot_inBetween 0 xs
    print . onehot_index $ onehot_inBetween 4 xs
    print . onehot_index $ onehot_inBetween 7 xs
    print . onehot_index $ onehot_inBetween 8 xs

    let nt = range 1 10
    print nt
    print $ find_inBetween 6 nt

    let nt = range 11 20
    print nt
    print $ find_inBetween 6 nt

    let nnt = Tensor [(range 1 10), (range 11 20)]
    print nnt
    print $ find_inBetween 6 nnt
    print $ find_inBetween_index 6 nnt

    let nt = accumulate $ Tensor [(range 1 10), (range 11 20)]
    print nt
