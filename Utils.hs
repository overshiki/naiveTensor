module NaiveTensor.Utils (listSelect, listSplitAt, groupBy) where 

listSelect :: [Int] -> [a] -> [a]
listSelect xs alist = map (alist!!) xs 

listSplitAt :: Int -> [a] -> [[a]]
listSplitAt num ax = _listSplitAt num [] ax 
        where 
            _listSplitAt num _head _tail@(x:xs) 
                    | length _head < num  = _listSplitAt num (_head ++ [x]) xs 
                    | otherwise           = [_head, _tail]
            _listSplitAt num _head [] = [_head, []]


groupBy :: Int -> [a] -> [[a]]
groupBy num ax
        | length ax >= num = 
                    let [h, t] = listSplitAt num ax in 
                    h:(groupBy num t)
        | length ax == 0   = []
        | otherwise        = [ax]


main :: IO ()
main = do 
    let a = [1..10]
        [ah, at] = listSplitAt 5 a 
    print ah 
    print at
    let b = groupBy 5 a 
    print b