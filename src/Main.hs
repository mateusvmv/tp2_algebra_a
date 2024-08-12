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
        -- Unbound pivot or even amount of set variables, ignore
        | not (ue!(i,i)) || even = s
        -- The pivot is set because it must complement the set variables and result in zero
        | otherwise = s // [(i, True)]
        where even = foldl xor True [ue!(i,j) && s!j | j <- [i+1..cols]]
    unbound = filter (not . (ue!) . \i -> (i, i)) [0..min lines cols]
    start = array (0, cols) $ map (,True) unbound
    s = foldl solveLine start (reverse [0..min lines cols])

-- Calcula o teto da raíz quadrada de um inteiro
-- utilizando do Método de Heron
ceilSqrt :: Integer -> Integer
ceilSqrt 0 = 0
ceilSqrt n = heron (fromInteger n :: Deci) where
    heron xi
        | abs d > 0.5 = heron (xi+d)
        | floor xi^2 < n = floor xi + 1
        | otherwise = floor xi
        where d = (fromInteger n - xi * xi) / 2 / xi

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

justFind f = head . filter f

-- Algoritmo de Tonelly-Shanks
tonelli :: Integer -> Integer -> Maybe (Integer, Integer)
tonelli n p
    | p <= 2 = Just (mod n 2, mod n 2)
    | legendre n p == 1 = Just (r, p - r)
    | otherwise = Nothing
    where
    pow = powMod p
    (s, q) = justFind (odd . snd) $ iterate (bimap (+1) (.>>. 1)) (0, p-1)
    z = (+ 1) . last $ takeWhile (\i -> legendre i p + 1 /= p) [1..p-1]
    initial = (s, pow z q, pow n $ (q+1) .>>. 1, pow n q)
    step (m, c, r, t) = (i, c', r', t') where
        i = fst . justFind ((==1) . snd) $ iterate (bimap (+ 1) (`pow` 2)) (0, t)
        b = pow c (shiftL 1 (m - i - 1))
        r' = mod (r*b) p
        c' = mod (b*b) p
        t' = mod (t*c') p
    condition (_, _, _, t) = mod (t - 1) p == 0
    (_,_,r,_) = justFind condition (iterate step initial)

-- lista de primos menores ou iguais a b e 
-- onde m é resíduo quadrático módulo p
-- esses são os únicos primos onde x^2 = m mod p tem solução
bPrimes :: Integer -> Integer -> [Integer]
bPrimes b m = [x | x <- takeWhile (<=b) primes, legendre m x == 1]

-- Fatoração de N com base B por tentativa de divisão
factorizeBSmooth :: Integer -> Integer -> [Integer]
factorizeBSmooth b = factors where
    factors m
        | m <= 1 || null fs = []
        | otherwise = head fs : factors (div m $ head fs)
        where fs = [x | x <- takeWhile (<=b) primes, mod m x == 0]

-- Merge duas listas ordenadas, remove elementos repetidos
merge a [] = a
merge [] b = b
merge (x:xs) (y:ys)
    | y == x = y : merge xs ys
    | y < x = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)

-- Calcula a^b mod m em O(log(b))
powMod m a b = iter 1 a b where
    a |* b = mod (a*b) m
    iter r _ 0 = r
    iter r a b = iter r' (a |* a) (b .>>. 1)
        where r' = if odd b then r |* a else r

-- Calcula a raíz quadrada de n módulo m via força bruta em O(m)
sqrtMod m n = find (\r -> mod (r*r) m == n') [0..m-1] where n' = mod n m

-- Calcula o produtório de uma lista módulo n
productMod n = foldl (\a b -> mod (a*b) n) 1

-- Recebe três inteiros, tais que a^2 = b^2 mod n, e retorna dois fatores de n
fermatMethod n a b = (f1, f2) where
    f1 = gcd (a + b) n
    f2 = gcd (abs (a - b)) n


-- Calcula o limite de fatoração B:
smoothnessBound n = ceiling . exp $ sqrt (0.5 * log n' * (log . log) n') where n' = fromInteger n

-- Recebe uma lista de candidatos cujo quadrado módulo n pode ser B-smooth
-- Retorna (x, y), com x² = y² mod n e x != y mod n
mergeFactors n b candidates = (x, y) where
    factorize = factorizeBSmooth $ toInteger b
    maybeFactors i = if product fs == j then Just (i, fs) else Nothing
        where j = mod (i*i) n; fs = factorize j
    countFactors = map (\l -> (fromInteger . head $ l, length l)). group
    zeroIndex :: [e] -> Array Int e
    zeroIndex l = listArray (0, length l - 1) l

    (roots, factors) = bimap zeroIndex zeroIndex
        . unzip
        . take (length smoothPrimes + 20)
        . map (second countFactors)
        $ mapMaybe maybeFactors candidates
    smoothPrimes = takeWhile (<=b) . map fromInteger $ primes

    primeIdx :: UArray Int Int
    primeIdx = array (0, b) (zip smoothPrimes [0..])

    lines = length smoothPrimes - 1
    cols = length roots - 1

    maybeOddExp s (p, k) = if odd k then Just ((primeIdx ! p, s), True) else Nothing
    matrix :: UArray (Int, Int) Bool
    matrix = array ((0, 0), (lines, cols))
        . concatMap catMaybes
        . zipWith (map . maybeOddExp) [0 .. cols]
        $ elems factors

    solution = solve matrix
    included = case findIndex (all (even . snd)) (elems factors) of
        Just i -> [i]
        Nothing -> filter (solution!) [0 .. cols]

    factorCount :: UArray Int Int
    factorCount = accumArray (+) 0 (0, lines) includedFactors where
        includedFactors = map (first (primeIdx !))
            $ concatMap (factors !) included
    mergedFactors = map (first (smoothPrimes !!)) $ assocs factorCount

    x = productMod n $ map (roots !) included
    y = productMod n
        . map (uncurry (powMod n) . bimap toInteger (.>>. 1))
        $ mergedFactors

sanityCheck n = elem n (takeWhile (<=n) primes) || s*s == n || case quadraticSieve n of
    (Just (a, b), Just (x, y)) -> (a /= 1 && a /= n) || (b /= 1 && b /= n)
    _ -> False
    where s = ceilSqrt n
findFailure = find (not . sanityCheck) [2..]

isSquare n = r*r == n where r = ceilSqrt n

-- Filtro dos candidatos para a fatoração B-smooth
quadraticSieveSeg n indices len r = sfs where
    logs :: UArray Integer Double
    logs = accumArray (+) 0 (r, r+len-1) indices
    condition a l = b /= 0 && (isSquare b || l  >= logB) where
        b = mod (a^2) n
        logB = logBase 2 . fromInteger $ (a^2 - n)
    sfs = map fst
        . filter (uncurry condition)
        $ assocs logs

-- Calcula dois fatores de um inteiro n utilizando o algoritmo do Crivo Quadrático
quadraticSieve :: Integer -> (Maybe (Integer, Integer), Maybe (Integer, Integer))
quadraticSieve n
    | isSquare n = (Just (r, r), Nothing)
    | (a /= 1 && a /= n) || (b /= 1 && b /= n) = (Just (a, b), Just (x, y))
    | otherwise = traceShow (x,y) (Nothing, Nothing)
    where
    bound = smoothnessBound n
    r = ceilSqrt n
    iPrimes = bPrimes (toInteger bound) n
    len = shiftL 1 10
    indices :: (Integer, Integer, Integer) -> [(Integer, Double)]
    indices (p, ia, ib) = map (,logBase 2 $ fromInteger p) $ merge a b where
        m = p - mod r p
        repeat i = [r + i, r + i + p .. n-1]
        [a, b] = map (repeat . (`mod` p) . (+m)) [ia, ib]
    step (_, (i, r)) = (sfs, (i', r')) where
        sfs = quadraticSieveSeg n (concat i'') len r
        r' = r + len
        (i'', i') = unzip $ map (span ((<r') . fst)) i
    primePowers = concatMap (\p -> takeWhile (<n) $ iterate (*p) p) iPrimes
    addSolution p = (\(ia, ib) -> (p, ia, ib)) <$> tonelli n p
    solutions = mapMaybe addSolution primePowers
    candidates = concatMap fst
        . takeWhile ((<n+len) . snd . snd)
        . iterate step
        $ ([], (map indices solutions, r))
    (x, y) = mergeFactors n bound candidates
    (a, b) = fermatMethod n x y

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
    case quadraticSieve n of
        (Just (f1, f2), Just (x, y)) -> do
            -- imprime os números encontrados pelo Crivo Quadrático
            putStrLn $ "x = " ++ show x ++ " y = " ++ show y
            -- imprime os fatores de n
            putStrLn $ "Fatores: " ++ show f1 ++ ", " ++ show f2
        (Just (f1, f2), Nothing) -> do
            putStrLn "O algoritmo do Crivo Quadrático não foi utilizado!"
            -- imprime os fatores de n
            putStrLn $ "Fatores: " ++ show f1 ++ ", " ++ show f2
        (Nothing, Nothing) ->
            putStrLn "A fatoração falhou"
    return ()
