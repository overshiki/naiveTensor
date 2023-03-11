module NaiveTensor.Numeric (toMatrix, fromMatrix) where 
import NaiveTensor.NTensor 
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((><), Matrix, R)
import GHC.Float (float2Double, double2Float)

-- only support 2D NaiveTensor 
toMatrix :: NaiveTensor Float -> Matrix R
toMatrix nt = (x><y) values :: Matrix R
        where 
            [x,y] = size nt
            values = map float2Double (flatten2list nt)

fromMatrix :: Matrix R -> NaiveTensor Float
fromMatrix ma = fromList [x,y] values
        where
            (x,y) = LA.size ma 
            values = map double2Float (LA.toList (LA.flatten ma))

-- ATTENTION!!!, not all matrix are invertable
inv :: NaiveTensor Float -> NaiveTensor Float
inv = fromMatrix . LA.inv . toMatrix

flatten2Matrix :: [Int] -> NaiveTensor Float -> Matrix R 
flatten2Matrix [x,y] nt = (x><y) values :: Matrix R
        where 
            values = map float2Double (flatten2list nt)




main :: IO ()
main = do 
    let a = (2><2) [1..4] :: Matrix Float
    print a

    let a = ones [2,2]
        ma = toMatrix a 
    print a
    print ma

    let a = list2flatten [-1, 1.5, 1, -1]
        ma = flatten2Matrix [2,2] a 
    print ma
    print $ LA.inv ma
    print $ LA.toList (LA.flatten ma)
    print $ LA.size ma

    print $ fromMatrix ma

    -- let a = (2><2) [-1, 1.5, 1, -1] :: Matrix Float
        -- b = inv a 
    -- print b

    let a = fromList [2,2] [-1, 1.5, 1, -1]
        b = inv a 
    print b