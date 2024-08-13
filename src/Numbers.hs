module Numbers where
import Data.Fixed
import Data.Array.Base
import Data.Bits
import Data.Bifunctor
import Data.List

-- Calcula a^b mod m em O(log(b))
powMod m a b = iter 1 a b where
    a |* b = mod (a*b) m
    iter r _ 0 = r
    iter r a b = iter r' (a |* a) (b .>>. 1)
        where r' = if odd b then r |* a else r

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

-- Resolve x² = n mod p^k, dado m = p^(k-1), com ia e ib sendo soluções para x² = n mod m
upliftTonelli _ _ Nothing = Nothing
upliftTonelli n p (Just (m, (ia, ib))) = join ia' ib' where
    m' = m*p
    scan = find (\x -> mod (x*x) m' == mod n m')
    (ia', ib') = (scan [ia, ia+m .. m'-1], scan [ib, ib+m .. m'-1])
    join (Just ia) (Just ib) = Just (m', (ia, ib))
    join _ _ = Nothing

-- Calcula a raíz quadrada de n módulo m via força bruta em O(m)
sqrtMod m n = find (\r -> mod (r*r) m == n') [0..m-1] where n' = mod n m

-- Calcula o produtório de uma lista módulo n
productMod n = foldl (\a b -> mod (a*b) n) 1

-- Recebe três inteiros, tais que a^2 = b^2 mod n, e retorna dois fatores de n
fermatMethod n a b = (f1, f2) where
    f1 = gcd (a + b) n
    f2 = gcd (abs (a - b)) n
