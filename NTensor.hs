{-# LANGUAGE ScopedTypeVariables #-}

module NTensor (NaiveTensor(..), 
                flattenOne, ones, range,
                unwrap2list, 
                flatten,
                wraplift,
                eye, zeros, onehot,
                size,
                bproduct, bsum, lsum, badd,
                tconcat, t2concat,
                sumproduct, fconcat,
                transpose
                ) where

import System.Random
import Control.Monad (replicateM)

-- data type
data NaiveTensor a = Tensor [NaiveTensor a] | Leaf a | Null 
-- deriving (Show)


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
    | s < e = (range (s+1) e) <> (Leaf s)
    | s == e = Tensor []


eye :: Num a => Int -> NaiveTensor a 
eye l = Tensor (map (onehot l) [0..l-1])


-- utils
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

-- ready for sumproduct

bcall :: Num a => (NaiveTensor a -> NaiveTensor a -> NaiveTensor a) 
                    -> NaiveTensor a -> NaiveTensor a 
                    -> NaiveTensor a
-- do broadcast for the higher layer
bcall bproduct (Tensor xs) (Tensor ys) = Tensor as 
    where 
        zipxy = zip xs ys
        _bproduct (x, y) = bproduct x y
        as = map _bproduct zipxy

bproduct :: Num a => NaiveTensor a -> NaiveTensor a -> NaiveTensor a
bproduct (Tensor xs) (Tensor ys) = bcall bproduct (Tensor xs) (Tensor ys)
bproduct (Leaf x) (Leaf y) = Leaf (x*y)


badd :: Num a => NaiveTensor a -> NaiveTensor a -> NaiveTensor a 
badd (Tensor xs) (Tensor ys) = bcall badd (Tensor xs) (Tensor ys)
badd (Leaf x) (Leaf y) = Leaf (x+y)

lsum :: Num a => [NaiveTensor a] -> NaiveTensor a
lsum (ax@((Leaf x):xs)) = foldl badd (Leaf 0) ax

bsum :: Num a => NaiveTensor a -> NaiveTensor a 
bsum (Tensor xs) = Tensor (_sum xs) 
    -- do pattern matching of list type using list constructor
    where _sum ax@((Leaf x):xs) = [lsum ax]
          _sum ax@((Tensor x):xs) = map bsum ax 
          
sumproduct :: Num a => NaiveTensor a -> NaiveTensor a -> NaiveTensor a 
sumproduct x y = bsum $ bproduct x y

-- sumproductAt :: Num a => Int -> Int -> NaiveTensor a -> NaiveTensor a 



main :: IO ()
main = do 
    x <- return $ (wraplift $ ones [3]) <> (wraplift $ zeros [3])
    print x 

    y <- return $ transpose x 
    print y    
    y <- return $ transpose $ wraplift x 
    print y    
    y <- return $ transposeAt 1 (wraplift x) 
    print y 
    y <- return $ transposeAt 2 (wraplift x) 
    print y 

    print (transpose_schedule [2,1,0])
    print $ wraplift x
    print (transposeFor [0,2,1] (wraplift x))