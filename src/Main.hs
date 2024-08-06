module Main where
import Data.Bits
import Data.Array.Base
import Data.Array.Unboxed
import Data.Int
import Data.List
import Debug.Trace

upperEchelon :: UArray Int Int64 -> UArray Int Int64
upperEchelon mat = listArray (0, 63) (reverse psList) where
    findPivot [] _ = (Nothing, [])
    findPivot (r:rs) k
        | testBit r k = (Just r, rs)
        | otherwise = (r', r:mat') where (r', mat') = findPivot rs k
    takePivot p k r = if testBit r k then xor r p else r
    nextPivot (mat, ps) k = case findPivot mat k of
        (Just p, mat') -> (map (takePivot p k) mat', p:ps)
        (Nothing, mat') -> (mat', 0:ps)
    (_, psList) = foldl nextPivot (elems mat, []) [0..63]

solve :: UArray Int Int64 -> Int64
solve mat = s where
    ue = upperEchelon mat
    (h, n) = bounds ue
    solveBit (i, r) s
        -- Unbound pivot, set to one
        | r == 0 = setBit s i
        -- The pivot is set because it must complement the set variables and result in zero
        | odd $ popCount ((r .&. s) .>>. (i+1)) = setBit s i
        -- If there is an even amount of set variables, the pivot is unset
        | otherwise = s
    s = foldr solveBit 0 (assocs ue)

getSetBits :: Int64 -> [Int]
getSetBits n = filter (testBit n) [0..63]

main :: IO ()
main = do
    putStrLn "Hello, world!"
