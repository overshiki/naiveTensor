module NaiveTensor.Order (
            find_inBetween_index
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
            | t < x = h:(_onehot_inBetween t xs Cold)
            | t >= x = Cold:(_onehot_inBetween t xs h)
_onehot_inBetween t [] h = []

onehot_inBetween :: Ord a => a -> [a] -> [OneHot]
onehot_inBetween t (x:xs)
            | t < x = _onehot_inBetween t xs Cold 
            | t >= x = _onehot_inBetween t xs Hot 
onehot_inBetween t [] = []

onehot_index :: [OneHot] -> Int 
onehot_index (Hot:xs) = 0
onehot_index (Cold:xs) = 1 + (onehot_index xs)
onehot_index [] = 0

find_inBetween :: Ord a => a -> (NaiveTensor a) -> [[OneHot]]
find_inBetween i (Tensor ax@(x@(Leaf _x):xs)) = (onehot_inBetween i (map get_content ax)):[]

find_inBetween i (Tensor ax@(x@(Tensor _x):xs)) = nonehot:(find_inBetween i next_a)
        where 
            (ind:indices) = map ((map union) . (find_inBetween i)) ax 
            nonehot = foldl (++) ind indices
            index = onehot_index nonehot
            next_a = ax !! index

find_inBetween_index :: Ord a => a -> (NaiveTensor a) -> [Int]
find_inBetween_index i xs = xx 
        where 
            onehot = find_inBetween i xs 
            xx = map onehot_index onehot


main :: IO ()
main = do 
    let xs = [1,2,3,4,5,6]
    print $ onehot_inBetween 0 xs
    print $ onehot_inBetween 4 xs
    print $ onehot_inBetween 7 xs
    print $ onehot_inBetween 8 xs

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
