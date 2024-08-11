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
import Control.Monad.RWS (MonadState(put))

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
solve mat = s where
    ue = upperEchelon mat
    (lines, cols) = snd $ bounds ue
    solveLine :: UArray Int Bool -> Int -> UArray Int Bool
    solveLine s i
        -- Unbound pivot, set to one
        | not $ ue!(i,i) = s // [(i, True)]
        -- The pivot is set because it must complement the set variables and result in zero
        | foldl xor False [ue!(i,j) && s!j | j <- [0..i-1]] = s // [(i, True)]
        -- If there is an even amount of set variables, the pivot is unset
        | otherwise = s
    s = foldl solveLine (array (0, cols) []) [0..min lines cols]

ceilSqrt :: Integer -> Integer
ceilSqrt n = heron (fromInteger n :: Deci) where
    heron xi
        | abs d > 0.5 = heron (xi+d)
        | floor xi ^ 2 == n = floor xi
        | otherwise = ceiling xi
        where d = (fromInteger n / xi - xi) / 2

-- Segmento do Crivo de Eratosthenes
-- Coprimos de primos em ps ∪ {2} entre a e b, a≅1 mod 2 e b≅1 mod 2
-- Em O(n*log(log(n))) com n = b-a
sieveSeg a' b' ps = [i*2+1 | i <- [a..b], coprime ! i] where
    a = shiftR a' 1; b = shiftR b' 1
    muls p = [l, l+p .. b] where l = shiftR (p * ((a'+p-1) `div` p .|. 1)) 1
    coprime = accumArray (\_ b -> b) True (a, b) (map (,False) $ concatMap muls ps) :: UArray Integer Bool

-- Primos
-- Gera e armazena primos um segmento por vez
-- Em O(n*log(log(n))) com n sendo o maior primo gerado
primes :: [Integer] = [2,3] ++ sieve 5 where
    sieve n = sieveSeg n top ps ++ sieve (top+2) where
        top = min (n + 2^15) (2 + n*n - 4*n)
        ps = takeWhile (\p -> p*p < top) (tail primes)

-- calcula o simbolo de Legendre
legendre :: Integer -> Integer -> Integer
legendre a p = powMod p a ((p-1) `div` 2)

-- lista de primos menores ou iguais a b e 
-- onde m é resíduo quadrático módulo p
-- esses são os únicos primos onde x^2 = m mod p tem solução
bPrimes :: Integer -> Integer -> [Integer]
bPrimes b m = [x | x <- takeWhile (<=b) primes, legendre m x == 1]

factorizeBSmooth :: Integer -> Integer -> [Integer]
factorizeBSmooth b = factors where
    factors m
        | m <= 1 || null fs = []
        | otherwise = head fs : factors (div m $ head fs)
        where fs = [x | x <- takeWhile (<=b) primes, mod m x == 0]

powMod m a b = iter 1 a b where
    a |* b = mod (a*b) m
    iter r _ 0 = r
    iter r a b = iter r' (a |* a) (b .>>. 1)
        where r' = if odd b then r |* a else r

sqrtMod m n = find (\r -> mod (r*r) m == n') [0..m-1] where n' = mod n m

productMod n = foldl (\a b -> mod (a*b) n) 1

-- Takes three integers, such that a² = b² mod n, and yields two factors of n
fermatMethod n a b = (f1, f2) where
    f1 = gcd (a + b) n
    f2 = gcd (abs (a - b)) n


-- Calcula o limite de fatoração B:
smoothnessBound n = ceiling . exp $ sqrt (0.5 * log n' * (log . log) n') where n' = fromInteger n

-- Takes a list of candidates whose square modulo n could be smooth
-- Yields (x, y), with x² = y² mod n
mergeFactors n candidates = traceShow (lines, cols) (x, y) where
    b :: Int
    b = fromInteger $ smoothnessBound n

    factorize = factorizeBSmooth (toInteger b)
    maybeFactors i = if product fs == j then Just (i, fs) else Nothing
        where j = mod (i*i) n; fs = factorize j
    countFactors = map (\l -> (fromInteger . head $ l, length l)). group
    zeroIndex :: [e] -> Array Int e
    zeroIndex l = listArray (0, length l - 1) l
    (roots, factors) = bimap zeroIndex zeroIndex
        . unzip
        . take (20 + lines)
        . map (second countFactors)
        $ mapMaybe maybeFactors candidates    

    smoothPrimes = map fromInteger $ bPrimes (toInteger b) 1

    primeIdx :: UArray Int Int
    primeIdx = array (0, b) (zip smoothPrimes [0..])

    lines = length smoothPrimes
    cols = length roots - 1

    maybeOddExp s (p, k) = if odd k then Just ((primeIdx ! p, s), True) else Nothing
    matrix :: UArray (Int, Int) Bool
    matrix = array ((0, 0), (lines, cols))
        . concatMap catMaybes
        . zipWith (map . maybeOddExp) [0 .. cols]
        $ elems factors

    solution = solve matrix
    included = filter (solution!) [0 .. cols]

    factorCount :: UArray Int Int
    factorCount = accumArray (+) 0 (0, lines) includedFactors where
        includedFactors = map (first (primeIdx !))
            $ concatMap (factors !) included

    x = productMod n $ map (roots !) included
    y = productMod n
        . map ((uncurry (powMod n) . second (.>>. 1)) . first toInteger)
        $ assocs factorCount

quadraticSieve n = fermatMethod n a b where
    r = ceilSqrt n
    (a, b) = mergeFactors n [r .. n-1]

main :: IO ()
main = do
    -- lê um inteiro N >> 0
    n <- readLn
    -- calcula o limite da fatoração B
    let b = smoothnessBound n
    -- imprime o limite B
    putStrLn $ "B: " ++ show b
    -- imprime quantos primos serão usados no crivo
    putStrLn $ "Quantidade de Primos: " ++ show (length $ bPrimes b n)
    -- calcula os fatores de N
    let (f1, f2) = quadraticSieve n
    -- imprime os fatores de N
    putStrLn $ "Fatores: " ++ "x = " ++ show f1 ++ " y = " ++ show f2
    return ()
    
