module Main where

import Sieve

main :: IO ()
main = do
    -- lê um inteiro N >> 0
    n <- readLn
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
