

ioreduce :: IO [Float] -> IO Float -> IO [Float]
ioreduce xs x = xs >>= \xs -> x >>= \x -> return (xs ++ [x])

ioconcat :: IO [Float] -> [IO Float] -> IO [Float]
ioconcat ixs (x:xs) = ioconcat (ioreduce ixs x) xs
ioconcat ixs [] = ixs

genIO :: IO Float
genIO = do 
    return 1.0

genIO_list :: IO [Float]
genIO_list = do 
    return [2.0]

main :: IO ()
main = do 
    x <- ioreduce genIO_list genIO
    print x 
    x <- ioconcat genIO_list (map (\x->genIO) [1..5])
    print x
