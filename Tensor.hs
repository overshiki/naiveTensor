module Tensor (NaiveTensor, 
                flattenOne, 
                ones, 
                range,
                unwrap2list, 
                flatten
                ) where


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

genX :: Num a => a -> ([Int] -> NaiveTensor a)
genX a = _xs_func 
    where 
        _xs_func [x] = flattenX a x
        _xs_func (x:xs) = tensor_repeat x (_xs_func xs)        


ones :: Num a => [Int] -> NaiveTensor a 
ones = genX 1

zeros :: Num a => [Int] -> NaiveTensor a 
zeros = genX 0

onehot :: Num a => Int -> Int -> NaiveTensor a 
onehot l i = Tensor (map (construct i) [0..l-1])
    where construct i k 
                |i==k = Leaf 1
                |otherwise = Leaf 0



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


transpose :: NaiveTensor a -> NaiveTensor a 
transpose (Tensor xa@(x:xs)) = 
        Tensor $ map (ttake xa) [0..inner_len-1] 
    where 
        inner_len = length $ unwrap2list $ x

size :: NaiveTensor a -> [Int]
size (Tensor xs) = [length xs] ++ (size $ xs !! 0)
size (Leaf x) = []

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
    where _sum ax@((Leaf x):xs) = [lsum ax]
          _sum ax@((Tensor x):xs) = map bsum ax 
          
sumproduct :: Num a => NaiveTensor a -> NaiveTensor a -> NaiveTensor a 
sumproduct x y = bsum $ bproduct x y


ori_tensor = Tensor [(range 1 10), (range 1 10)]
trans_tensor = transpose ori_tensor
list1 = unwrap2list $ flatten $ ones [2, 2]

main = do
    print $ Tensor [Leaf 1, Leaf 2]
    print $ Tensor [Leaf 1 | _<-[1..3]]
    print $ flattenOne 3

    print $ fconcat (flattenOne 3) (flattenOne 4)
    -- sprint $ fconcat (flattenOne 3) (flattenOne 4)

    print $ tconcat $ take 3 (repeat $ flattenOne 2)

    print $ ones [2, 2]
    print $ t2concat $ ones [2, 2]

    print $ flatten $ ones [2, 2]

    print $ flatten $ ones [2, 2, 2]

    print $ range 1 10

    print $ length $ list1

    print $ ori_tensor
    print $ trans_tensor

    print $ size $ ori_tensor
    print $ size $ trans_tensor

    print $ mappend (flattenOne 3) (flattenOne 4)
    print $ (flattenOne 3) <> (flattenOne 4)

    print $ flatten $ ones [2, 2, 2, 2]

    print $ bproduct ori_tensor ori_tensor
    print $ badd ori_tensor ori_tensor

    print $ lsum list1

    print . size $ ori_tensor
    print $ bsum $ bproduct ori_tensor ori_tensor
    print $ sumproduct ori_tensor ori_tensor

    print (ones [2,2,2])
    print (zeros [2,2,2])

    print (onehot 3 1)
    print (eye 2)
    print (eye 3)

    -- print (from_list [1,2,3])
    -- print $ (read [1,2,3]) :: NaiveTensor
    -- print (from_string "[1,2,3]")

    print(fmap ( + 2) (eye 2))