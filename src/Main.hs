module Main where
import Data.Bits
import Data.Array.Base
import Data.Array.Unboxed
import Data.Int
import Data.List
import Debug.Trace
import Data.Fixed
import Data.Maybe
import Data.Bifunctor

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
    solveBit (i, r) s
        -- Unbound pivot, set to one
        | r == 0 = setBit s i
        -- The pivot is set because it must complement the set variables and result in zero
        | odd $ popCount ((r .&. s) .>>. (i+1)) = setBit s i
        -- If there is an even amount of set variables, the pivot is unset
        | otherwise = s
    s = foldr solveBit 0 (assocs ue)

ceilSqrt :: Integer -> Integer
ceilSqrt n = heron (fromInteger n :: Deci) where
    heron xi
        | abs d > 0.5 = heron (xi+d)
        | floor xi ^ 2 == n = floor xi
        | otherwise = ceiling xi
        where d = (fromInteger n / xi - xi) / 2

primes = 2 : sieve [3, 5..] where
    sieve (p:xs) = p : filter (\x -> mod x p /= 0) xs

factorizeBSmooth b = factors where
    factors m
        | m == 1 || null fs = []
        | otherwise = head fs : factors (div m $ head fs)
        where fs = [x | x <- [2..min b m], mod m x == 0]

powMod m a b = iter 1 a b where
    a |* b = mod (a*b) m
    iter r _ 0 = r
    iter r a b = iter r' (a |* a) (b .>>. 1)
        where r' = if odd b then r |* a else r

sqrtMod m n = find (\r -> mod (r*r) m == n') [0..m-1] where n' = mod n m

factorLim :: Int
factorLim = 127

productMod n = foldl (\a b -> mod (a*b) n) 1

-- Takes three integers, such that a² = b² mod n, and yields two factors of n
fermatMethod n a b = (f1, f2) where
    f1 = gcd (a + b) n
    f2 = gcd (abs (a - b)) n

-- Takes a list of candidates whose square modulo n could be smooth
-- Yields (x, y), with x² = y² mod n
mergeFactors n candidates = (x, y) where
    factorize = factorizeBSmooth (toInteger factorLim)
    maybeFactors i = if product fs == j then Just (i, fs) else Nothing
        where j = mod (i*i) n; fs = factorize j
    (roots, factors) = unzip
        . take 63 -- The sign bit will break the algorithm
        . map (second (map (\l -> (fromInteger . head $ l, length l)). group))
        $ mapMaybe maybeFactors candidates

    bounds = (0, factorLim);
    bits = [0 .. length roots - 1] -- We assign each root to a bit

    maybeOddExp s (p, k) = if odd k then Just (p, s) else Nothing
    matrix :: UArray Int Int64
    matrix = accumArray (\a -> xor a . bit) 0 bounds
        . concatMap catMaybes
        . zipWith (map . maybeOddExp) bits
        $ factors

    solution = solve matrix
    included = filter (testBit solution) bits

    factorCount :: UArray Int Int
    factorCount = accumArray (+) 0 bounds includedFactors where
        includedFactors = concatMap (factors !!) included

    x = productMod n $ map (roots !!) included
    y = productMod n
        . map ((uncurry (powMod n) . second (.>>. 1)) . first toInteger)
        $ assocs factorCount

quadraticSieve n = fermatMethod n a b where
    r = ceilSqrt n
    (a, b) = mergeFactors n [r .. n-1]

main :: IO ()
main = do
    putStrLn "Hello, world!"
