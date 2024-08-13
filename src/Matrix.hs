module Matrix where
import Data.Array.Base
import Debug.Trace
import Data.Bits

upperEchelon :: UArray (Int, Int) Bool -> UArray (Int, Int) Bool
upperEchelon mat = listArray ((0, 0), (lines, cols)) (concatMap elems . reverse $ psList) where
    (lines, cols) = snd $ bounds mat
    lineList = [listArray (0, cols) [mat!(i,j) | j <- [0..cols]] | i <- [0..lines]]
    findPivot [] _ = (Nothing, [])
    findPivot (r:rs) k
        | r!k = (Just r, rs)
        | otherwise = (r', r:mat') where (r', mat') = findPivot rs k
    takePivot p k r = if r!k then listArray (0, cols) [xor (r!j) (p!j) | j <- [0..cols]] else r
    nextPivot :: ([UArray Int Bool], [UArray Int Bool]) -> Int -> ([UArray Int Bool], [UArray Int Bool])
    nextPivot (lines, ps) k = case findPivot lines k of
        (Just p, mat') -> (map (takePivot p k) mat', p:ps)
        (Nothing, mat') -> (mat', array (0, cols) []:ps)
    (_, psList) = foldl nextPivot (lineList, []) [0..cols]

solve :: UArray (Int, Int) Bool -> UArray Int Bool
solve mat = trace ("Resolvendo matriz de tamanho " ++ show (snd $ bounds mat)) s where
    ue = upperEchelon mat
    (lines, cols) = snd $ bounds ue
    solveLine :: UArray Int Bool -> Int -> UArray Int Bool
    solveLine s i
        -- Unbound pivot or even amount of set variables, ignore
        | not (ue!(i,i)) || even = s
        -- The pivot is set because it must complement the set variables and result in zero
        | otherwise = s // [(i, True)]
        where even = foldl xor True [ue!(i,j) && s!j | j <- [i+1..cols]]
    unbound = filter (not . (ue!) . \i -> (i, i)) [0..min lines cols]
    start = array (0, cols) $ map (,True) unbound
    s = foldl solveLine start (reverse [0..min lines cols])
