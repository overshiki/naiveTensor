module NaiveTensor.Order (IndexInBetween(..),
                matchSelect, find_inBetween
            ) where 
import NaiveTensor.NTensor

data Order = DFS | BFS
data IndexInBetween = Index Int | AllBelow | AllAbove | OutRange deriving (Eq, Show)

add :: IndexInBetween -> IndexInBetween -> IndexInBetween
add (Index x) (Index y) = Index (x+y)
add AllBelow (Index x) = Index x 
add AllBelow _ = OutRange
add (Index x) AllAbove = OutRange
add (Index x) _ = Index x
add OutRange (Index x) = Index x 
add AllAbove _ = OutRange
add OutRange _ = OutRange

match :: IndexInBetween -> IndexInBetween
match (Index x) = Index x 
match OutRange = OutRange
match AllAbove = OutRange
match AllBelow = OutRange

union :: [IndexInBetween] -> IndexInBetween
union (x:xs) = foldl add x xs 
union [] = OutRange

matchInBetween :: [IndexInBetween] -> IndexInBetween
matchInBetween (OutRange:xs) = (Index 1) `add` matchInBetween xs
matchInBetween (x@(Index _x):xs) = Index 1
matchInBetween [] = AllAbove

matchSelect :: IndexInBetween -> [a] -> Maybe a 
matchSelect (Index i) xs = Just (xs !! i)
matchSelect _ _ = Nothing

-- list [a] should be ordered smaller2larger:left2right
_index_inBetween :: Ord a => a -> [a] -> IndexInBetween
_index_inBetween target (x:xs)
        | target < x  = AllBelow
        | target >= x = (Index 1) `add` (_index_inBetween target xs)
_index_inBetween target [] = AllAbove

index_inBetween :: Ord a => a -> [a] -> IndexInBetween
index_inBetween target xs = match $ _index_inBetween target xs

find_inBetween :: Ord a => a -> (NaiveTensor a) -> [IndexInBetween]
find_inBetween i (Tensor ax@(x@(Leaf _x):xs)) = [xx]
        where
            xx = index_inBetween i (map get_content ax)

find_inBetween i (Tensor ax@(x@(Tensor _x):xs)) = index:(find_inBetween i next_a)
        where 
            minusOne (Index i) = Index (i-1)
            (ind:indices) = map (find_inBetween i) ax 
            nindices = foldl (++) ind indices
            index = minusOne $ matchInBetween nindices
            unpack (Just x) = x
            next_a = unpack $ matchSelect index ax


main :: IO ()
main = do 
    let xs = [1,2,3,4,5,6]
    print $ index_inBetween 4 xs
    print $ index_inBetween 7 xs
    let nt = range 1 10
    print nt
    print $ find_inBetween 6 nt
    let nnt = Tensor [(range 1 10), (range 11 20)]
    print nnt
    print $ find_inBetween 6 nnt