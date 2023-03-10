module NaiveTensor.Numeric (toMatrix) where 
import NaiveTensor.NTensor 
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra ((><), Matrix, R, inv)
import GHC.Float (float2Double)

-- only support 2D NaiveTensor 
toMatrix :: NaiveTensor Float -> Matrix R
toMatrix nt = (x><y) values :: Matrix R
        where 
            [x,y] = size nt
            values = map float2Double (flatten2list nt)

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
    print $ inv ma
    -- let a = (2><2) [-1, 1.5, 1, -1] :: Matrix Float
        -- b = inv a 
    -- print b