import NTensor


ori_tensor = Tensor [(range 1 10), (range 1 10)]
trans_tensor = transpose ori_tensor
list1 = unwrap2list $ flatten $ ones [2, 2]

main = do
    print $ Tensor [Leaf 1, Leaf 2]
    print $ Tensor [Leaf 1 | _<-[1..3]]
    print $ flattenOne 3

    -- print $ fconcat (flattenOne 3) (flattenOne 4)
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
    print $ wraplift $ flattenOne 3

