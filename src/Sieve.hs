module Sieve where

import Data.Bits
import Data.Array.Base
import Data.Array.Unboxed
import Data.Int
import Data.List
import Debug.Trace
import Data.Fixed
import Data.Maybe
import Data.Bifunctor

import Numbers
import Matrix

-- lista de primos menores ou iguais a b e 
-- onde m é resíduo quadrático módulo p
-- esses são os únicos primos onde x^2 = m mod p tem solução
bPrimes :: Integer -> Integer -> [Integer]
bPrimes m b = [x | x <- takeWhile (<=b) primes, legendre m x == 1]

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

-- Calcula o limite de fatoração B:
smoothnessBound n = ceiling r where
    n' = fromInteger n
    r = exp $ sqrt (0.5 * log n' * (log . log) n')

-- Recebe uma lista de candidatos cujo quadrado módulo n pode ser B-smooth
-- Retorna (x, y), com x² = y² mod n e x != y mod n
dixon n b candidates = (x, y) where
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
        Just i -> trace "Quadrado perfeito encontrado! Matriz não usada" [i]
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

-- sanityCheck n = elem n (takeWhile (<=n) primes) || s*s == n || case quadraticSieve n of
--     (Just (a, b), Just (x, y)) -> (a /= 1 && a /= n) || (b /= 1 && b /= n)
--     _ -> False
--     where s = ceilSqrt n
-- findFailure = find (not . sanityCheck) [2..]

isSquare n = r*r == n where r = ceilSqrt n

-- Filtro dos candidatos para a fatoração B-smooth
quadraticSieveSeg n indices len r = trace ("Segmento do crivo de " ++ show r ++ " a " ++ show (r+len-1) ++ " com " ++ show (length sfs) ++ " candidatos encontrados") sfs where
    logs :: UArray Integer Double
    logs = accumArray (+) 0 (r, r+len-1) indices
    condition a l = b /= 0 && (isSquare b || l  >= logB) where
        b = mod (a^2) n
        logB = logBase 2 . fromInteger $ (a^2 - n)
    sfs = map fst
        . filter (uncurry condition)
        $ assocs logs

quadraticSieveCandidates n iPrimes = candidates where
    r = ceilSqrt n
    len = shiftL 1 14
    indices :: (Integer, (Integer, Integer)) -> [(Integer, Double)]
    indices (p, (ia, ib)) = map (,logBase 2 $ fromInteger p) $ merge a b where
        m = p - mod r p
        repeat i = [r + i, r + i + p .. n-1]
        [a, b] = map (repeat . (`mod` p) . (+m)) [ia, ib]
    primePowers' p (ia, ib) = catMaybes
        . takeWhile (maybe False ((<maximum iPrimes) . fst))
        $ iterate (upliftTonelli n p) (Just (p, (ia, ib)))
    primePowers p = maybe [] (primePowers' p) (tonelli n p)
    solutions = trace ("Primos e potências usados no crivo: " ++ show (length s)) s where
        s = concatMap primePowers iPrimes
    step (_, (i, r)) = (sfs, (i', r')) where
        sfs = quadraticSieveSeg n (concat i'') len r
        r' = r + len
        (i'', i') = unzip $ map (span ((<r') . fst)) i
    candidates = concatMap fst
        . takeWhile ((<n+len) . snd . snd)
        . iterate step
        $ ([], (map indices solutions, r))

quadraticSieveAttempt n bound
    | (a /= 1 && a /= n) || (b /= 1 && b /= n) = Just ((a, b), (x, y))
    | otherwise = Nothing
  where
    primes = bPrimes n (toInteger bound)
    candidates = trace ("Gerados " ++ show primeCount ++ " primos com B = " ++ show bound) c where
        primeCount = length . takeWhile (<=toInteger bound) $ primes
        c = quadraticSieveCandidates n primes
    (x, y) = dixon n bound candidates
    (a, b) = fermatMethod n x y

-- Calcula dois fatores de um inteiro n utilizando o algoritmo do Crivo Quadrático
quadraticSieve :: Integer -> (Maybe (Integer, Integer), Maybe (Integer, Integer))
quadraticSieve n
    | isSquare n = (Just (r, r), Nothing)
    | null success = (Nothing, Nothing)
    | otherwise = (Just (a, b), Just (x, y))
  where
    r = ceilSqrt n
    bound = smoothnessBound n
    attempts = map (quadraticSieveAttempt n) [bound, bound*2..]
    success = catMaybes attempts
    ((a, b), (x, y)) = head success
