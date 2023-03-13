module NaiveTensor.Matrix (toMatrix, 
                            fromMatrix, 
                            inv,
                            tailMatrix,
                            (@@),
                            matrixHSplitAt,
                            matrixVSplitAt,
                            matrixSplitAt
                            ) where 
import NaiveTensor.NTensor 
import NaiveTensor.Reduce
import NaiveTensor.Transpose
import NaiveTensor.Utils
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

-- Note this only works for matrix(2D NaiveTensor)
tailMatrix :: NaiveTensor a -> NaiveTensor a 
tailMatrix (Tensor ax@((Tensor x):xs)) = Tensor (map _tail xs)
        where 
            _tail (Tensor (y:ys)) = Tensor ys 


matrixHSplitAt :: Int -> NaiveTensor a -> [NaiveTensor a]
matrixHSplitAt outer (Tensor ax)
    = [tl, tr]
        where 
            [outer_up, outer_down] = listSplitAt outer ax
            tl = Tensor outer_up 
            tr = Tensor outer_down 

data Noder = Fst | Snd deriving (Show, Eq)

matrixVSplitAt :: Int -> NaiveTensor a -> [NaiveTensor a]
matrixVSplitAt inner at@(Tensor ax)
    = [tu, td]
        where 
            fetch x (Tensor nx) = Tensor $ (listSplitAt inner nx) !! x

            inner_left = map (fetch 0) ax
            inner_right = map (fetch 1) ax

            tu = Tensor inner_left
            td = Tensor inner_right


-- default order is upper_left, upper_right, down_left, down_right 
matrixSplitAt :: (Int, Int) -> NaiveTensor a -> [NaiveTensor a]
matrixSplitAt (outer, inner) nt 
    = [ul, ur, dl, dr]
        where 
            [outer_up, outer_down] = matrixHSplitAt outer nt 
            [ul, ur] = matrixVSplitAt inner outer_up
            [dl, dr] = matrixVSplitAt inner outer_down




flatten2Matrix :: [Int] -> NaiveTensor Float -> Matrix R 
flatten2Matrix [x,y] nt = (x><y) values :: Matrix R
        where 
            values = map float2Double (flatten2list nt)

-- ATTENTION!!!, only support matrix 
(@@) :: (Num a) => NaiveTensor a -> NaiveTensor a -> NaiveTensor a 
(@@) nx ny = sumproduct nx (transpose ny)

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

    let b = fromList [3,3] [1,2,3,4,5,6,7,8,9]
    print b
    print $ tailMatrix b 

    let a = fromList [2,3] [1..6]
        b = fromList [3,2] [1..6]
    print $ size $ a @@ b
    print $ size $ b @@ a

    let v = fromList [3] [1..3]
        v2 = fromList [2] [1..2]
    print $ v @@ b
    let res = (v @@ b) @@ v2
    print res 
    let nres = (flatten2list res) !! 0
    print nres

    let a = fromList [4,4] [1..16]
        [u,d] = matrixHSplitAt 2 a 
        [l,r] = matrixVSplitAt 2 a 
    print "u"
    print u
    print "d"
    print d
    print "l"
    print l 
    print "r"
    print r
    let [lu,ld,ru,rd] = matrixSplitAt (2,2) a 
    print lu 
    print ld 
    print ru 
    print rd
    print "done"