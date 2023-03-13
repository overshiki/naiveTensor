module NaiveTensor.Broadcast (
            zipcall,
            bproduct, badd,
            bsum, squeezeEnd,
            _sumproduct, _sumproductAt,
            listlift, tappend, cartesian
) where

import NaiveTensor.NTensor 
import NaiveTensor.Transpose 

-- ready for sumproduct
-- previously called bcall 
zipcall :: Num a => (NaiveTensor a -> NaiveTensor a -> NaiveTensor a) 
                    -> NaiveTensor a -> NaiveTensor a 
                    -> NaiveTensor a
-- do broadcast for the higher layer
zipcall bfunc (Tensor xs) (Tensor ys) = Tensor as 
    where 
        zipxy = zip xs ys
        _bfunc (x, y) = bfunc x y
        as = map _bfunc zipxy

bproduct :: Num a => NaiveTensor a -> NaiveTensor a -> NaiveTensor a
bproduct (Tensor xs) (Tensor ys) = zipcall bproduct (Tensor xs) (Tensor ys)
bproduct (Leaf x) (Leaf y) = Leaf (x*y)

-- broadcast add
badd :: Num a => NaiveTensor a -> NaiveTensor a -> NaiveTensor a 
badd (Tensor xs) (Tensor ys) = zipcall badd (Tensor xs) (Tensor ys)
badd (Leaf x) (Leaf y) = Leaf (x+y)


lsum :: Num a => [NaiveTensor a] -> NaiveTensor a
lsum (ax@((Leaf x):xs)) = foldl badd (Leaf 0) ax


-- broadcast sum 
bsum :: Num a => NaiveTensor a -> NaiveTensor a 
bsum (Tensor xs) = Tensor (_sum xs) 
    -- do pattern matching of list type using list constructor
    where _sum ax@((Leaf x):xs) = [lsum ax]
          _sum ax@((Tensor x):xs) = map bsum ax 


squeezeEnd :: NaiveTensor a -> NaiveTensor a 
squeezeEnd (Tensor xs@((Tensor x):a)) = Tensor (map squeezeEnd xs)
squeezeEnd as@(Tensor (xs@((Leaf x):(Leaf y):a))) = as 
squeezeEnd (Tensor xs@([Leaf x])) = Leaf x

-- do sumproduct at index_a -> index_b -> NT_a -> NT_b 
-- ATTENTION: this is the normal sumproduct: (x0, x1) @ (y0, y1) -> (x0, y0). Instead, it is defined such that: (x0, x1) @ (y0, y1) -> (x0) where x0==y0, x1==y1. 
-- TODO: rename this function please
_sumproduct :: Num a => NaiveTensor a -> NaiveTensor a -> NaiveTensor a 
_sumproduct x y = bsum $ bproduct x y

 
_sumproductAt :: Num a => Int -> Int -> NaiveTensor a -> NaiveTensor a -> NaiveTensor a
_sumproductAt index_a index_b nt_a nt_b = squeezeEnd $ _sumproduct x y 
        where 
            _size index nt = [0..index-1] ++ [index+1..(ndim nt)-1] ++ [index]
            size_a = _size index_a nt_a
            size_b = _size index_b nt_b 
            x = transposeFor size_a nt_a 
            y = transposeFor size_b nt_b




-- utils with fmap

listlift :: NaiveTensor a -> NaiveTensor [a]
listlift nt = fmap (\x -> [x]) nt

tappend :: a -> NaiveTensor [a] -> NaiveTensor [a]
tappend item nt = fmap (\x->item:x) nt

cartesian :: [[a]] -> NaiveTensor [a] 
cartesian (x:y:xs) = Tensor (tp x remain) 
        where 
            _len = length x 
            remain = take _len (repeat (cartesian (y:xs)))
            _tappend xis ps i = tappend (xis !! i) (ps !! i)
            tp xis ps = map (_tappend xis ps) [0..(length xis)-1]

cartesian [xs] = listlift $ Tensor (map Leaf xs)

main :: IO ()
main = do 
    x <- return $ ones [3,4,5]
    y <- return $ ones [5,3,4]

    z <- return $ _sumproductAt 2 0 x y 
    print $ size z    

    print $ listlift $ ones [2,2]

    print $ tappend 1 (listlift $ ones [2,2])
    print $ cartesian [["a","b"], ["c", "d"]]