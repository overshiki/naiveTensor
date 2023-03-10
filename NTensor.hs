{-# LANGUAGE ScopedTypeVariables #-}

module NaiveTensor.NTensor (NaiveTensor(..), 
                flattenOne, ones, zeros, range,
                eye, onehot,
                size, ndim,
                unwrap2list, flatten, wraplift, flatten2list, list2flatten,
                tconcat, t2concat,
                get_content, tsum,
                tselect
                ) where

import System.Random
import Control.Monad (replicateM)

-- data type
data NaiveTensor a = Tensor [NaiveTensor a] | Leaf a | Null  deriving (Eq)

-- Monoid
fconcat :: NaiveTensor a -> NaiveTensor a -> NaiveTensor a 
fconcat (Tensor x) ya@(Leaf y) = Tensor $ x ++ [ya]
fconcat ya@(Leaf y) (Tensor x) = Tensor $ [ya] ++ x 
fconcat (Tensor x) (Tensor y) = Tensor $ x ++ y 
fconcat xa@(Leaf x) ya@(Leaf y) = Tensor $ [xa, ya]
fconcat x Null = x 
fconcat Null x = x


instance Semigroup (NaiveTensor a) where 
    (<>) = fconcat

instance Monoid (NaiveTensor a) where
    mempty = Null 


-- Show
instance Show a => Show (NaiveTensor a) where 
    show (Leaf x) = show x
    show (Tensor (x:xs)) =  (foldl scat init ax) ++ "]" 
        where
            scat x y = x ++ ", " ++ y
            init = "[" ++ (show x) 
            ax = map show xs

    show Null = "Null"

-- Functor
instance Functor NaiveTensor where
    fmap f (Tensor x) = Tensor (map (fmap f) x)
    fmap f (Leaf x)   = Leaf (f x)
    fmap f Null       = Null

-- -- Read
-- -- from_string x = from_list (read x ::[Int]) 
-- from_string :: Num a => [Char] -> NaiveTensor a -> NaiveTensor a
-- from_string (x:xs) a@(Tensor content)
--         | x=="[" = Tensor (from_string xs [a])
--         | x=="]" = Tensor content
--         | otherwise = from_string xs (Tensor ([Leaf (read x)] ++ content))

-- instance Read a => Read (NaiveTensor a) where
--     read x@(xs:sx) = Tensor (map Leaf x)


-- utils 
size :: NaiveTensor a -> [Int]
size (Tensor xs) = [length xs] ++ (size $ xs !! 0)
size (Leaf x) = []

ndim :: NaiveTensor a -> Int 
ndim x = length $ size x


-- constructors
tensor_repeat :: Num a => Int -> NaiveTensor a -> NaiveTensor a
tensor_repeat x t = Tensor a where a = take x (repeat t)

flattenX :: Num a => a -> Int -> NaiveTensor a 
flattenX a l = tensor_repeat l (Leaf a)

-- flattenOne x = tensor_repeat x (Leaf 1)
flattenOne :: Num a => Int -> NaiveTensor a
flattenOne = flattenX 1

flattenZero :: Num a => Int -> NaiveTensor a
flattenZero = flattenX 0


genX :: forall a. Num a => a -> ([Int] -> NaiveTensor a)
genX a = _xs_func 
    where 
        _xs_func :: [Int] -> NaiveTensor a
        _xs_func [x] = flattenX a x
        _xs_func (x:xs) = tensor_repeat x (_xs_func xs)        


ones :: Num a => [Int] -> NaiveTensor a 
ones = genX 1

zeros :: Num a => [Int] -> NaiveTensor a 
zeros = genX 0

onehot :: Num a => Int -> Int -> NaiveTensor a 
onehot l i = Tensor (map (construct i) [0..l-1])
    where construct i k 
                | i==k = Leaf 1
                | otherwise = Leaf 0



range :: (Num a, Eq a, Ord a) => a -> a -> NaiveTensor a
range s e 
    | s < e = (Leaf s) <> (range (s+1) e)
    | s == e = Tensor []


eye :: Num a => Int -> NaiveTensor a 
eye l = Tensor (map (onehot l) [0..l-1])



-- reduce sum to Num 
tsum :: Num a => NaiveTensor a -> a 
tsum (Tensor ax@((Leaf x):xs)) = sum (map get_content ax)
tsum (Tensor ax@((Tensor x):xs)) = sum (map tsum ax)







unwrap2list :: NaiveTensor a -> [NaiveTensor a] 
unwrap2list (Tensor x) = x 
unwrap2list xa@(Leaf x) = [xa]

-- x -> Tensor [x]
wraplift :: NaiveTensor a -> NaiveTensor a 
wraplift x = Tensor [x]


tconcat :: [NaiveTensor a] -> NaiveTensor a 
tconcat (x:xs) = foldl mappend x xs

t2concat :: NaiveTensor a -> NaiveTensor a 
t2concat (Tensor xs) = tconcat xs 

flatten :: NaiveTensor a -> NaiveTensor a 
flatten (Tensor (x:xs)) = foldl flatten_concat x xs
        where 
        -- flatten_concat :: NaiveTensor a -> NaiveTensor a -> NaiveTensor a
            flatten_concat xa@(Tensor xx) ya@(Tensor yy) = (flatten xa) <> (flatten ya)
            flatten_concat xa@(Leaf xx) ya@(Leaf yy) = Tensor [xa, ya]
            flatten_concat (Tensor xx) ya@(Leaf yy) = Tensor (xx ++ [ya]) 

-- select by indices 
tselect :: [Int] -> (NaiveTensor a) -> a 
tselect (i:is) (Tensor xs) = tselect is (xs !! i)
tselect [] (Leaf x) = x

-- back door for Leaf 
get_content :: NaiveTensor a -> a 
get_content (Leaf x) = x

flatten2list :: NaiveTensor a -> [a]
flatten2list (Tensor ax@((Leaf x):xs)) = map get_content ax
flatten2list nt@(Tensor ax@((Tensor x):xs)) = flatten2list (flatten nt)

list2flatten :: [a] -> NaiveTensor a 
list2flatten xs = Tensor (map Leaf xs)

main :: IO ()
main = do 
    x <- return $ (wraplift $ ones [3]) <> (wraplift $ zeros [3])
    print x 

    print $ tsum (ones [2,2,2])

    print $ tselect [1,1,1] (ones [2,2,2])

    print $ ((ones [2,2])::(NaiveTensor Float))

    
