{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE ExplicitForAll #-}
-- {-# LANGUAGE RankNTypes #-}
-- nested_apply :: forall a b. (Ord a, Ord b) => (a -> a -> a) -> (NaiveTensor b) -> b
-- f :: [a] -> [a]
f :: forall a. [a] -> [a]
f xs = ys ++ ys
  where ys :: [a]
        ys = reverse xs


main :: IO ()
main = do
    x <- return (f [1,2,3])
    -- let x = [1,2,3]
    -- x <- return [1,2,3]
    print x