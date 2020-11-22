module Resolvedor (
    resolve,
    noIntervalo,
    buscaGrupo,
    buscaLinCol
) where

import Matriz
import Data.List (sort)
import Puzzle


resolve :: Int -> Int -> Int -> [Int] -> IO()
resolve i j num matriz =
    if (getPos Puzzle.principal i j) == 0 then
        if (possivel num i j matriz) then
            recursao i j num (Matriz.setPos num (j+(n-1)*i) matriz)
        else (resolve i j (num-1) matriz)
    else recursao i j num matriz

recursao :: Int -> Int -> Int -> [Int] -> IO()
recursao i j num matriz
    | (i == Puzzle.n-1) && (j == Puzzle.n-1) = print (matriz) -- fim
    | (j == Puzzle.n-1) = resolve (i+1) 0 Puzzle.n matriz
    | otherwise = resolve i (j+1) Puzzle.n matriz


-- -=-=-=-=-=-=-=-=- Aqui estão as funções auxiliares -=-=-=-=-=-=-=-=-


-- Busca por elemento x num grupo
buscaGrupo :: Int -> Int -> Bool
buscaGrupo x grupo = x `elem` (Matriz.getGrupo grupo (Matriz.achatarMatriz Puzzle.secundaria))

-- Busca por elemento x na posição [lin][col]
buscaLinCol :: Int -> Int -> Int -> [Int] -> Bool
buscaLinCol x lin col matriz = x `elem` (Matriz.getLinha (lin*Puzzle.n) matriz) || x `elem` (Matriz.getColuna col matriz)

-- Verifica se é possível inserir "num" na posição [i][j]
possivel :: Int -> Int -> Int -> [Int] -> Bool
possivel 0 _ _ _ = False
possivel num i j matriz = (not ((buscaGrupo num (Matriz.getPos Puzzle.secundaria i j)) || (buscaLinCol num i j matriz)))
                          && (noIntervalo (Matriz.getGrupo (Matriz.getPos Puzzle.secundaria i j) (Matriz.achatarMatriz Puzzle.secundaria)) ((Matriz.tamGrupos (replicate (Matriz.numGrupos (Matriz.achatarMatriz Puzzle.secundaria)) 0) (Matriz.achatarMatriz Puzzle.secundaria)) !! (Matriz.getPos Puzzle.secundaria i j)) num)

-- Verifica se um numero está no intervalo possivel do grupo
noIntervalo :: [Int] -> Int -> Int -> Bool
noIntervalo grupo tamanho valor
    |  grupo == [] = True
    |  (length grupo) == tamanho - 1 = ehSequencia (valor:grupo)
    |  otherwise = (valor >= ((Matriz.minimo grupo) - (tamanho - (distancia grupo)))) && (valor <= ((Matriz.maximo grupo) - (tamanho - (distancia grupo))))

-- Confere se existe uma sequência de números em uma lista
ehSequencia :: [Int] -> Bool
ehSequencia [] = True
ehSequencia lista =
    if (sort lista) == [((sort lista)!!0)..(last(sort lista))] then
        True
    else
        False

distancia :: [Int] -> Int
distancia grupo = (Matriz.maximo grupo) - (Matriz.minimo grupo) + 1

-- Metodo que printa a matriz remontada
printMatriz :: [Int] -> Int -> Int -> IO ()
printMatriz [] _ _ = putStr "\n" 
printMatriz (x:xs) n v = 
    do
        putStr " " 
        if n - 1 == v 
            then do 
                putStrLn (show x)
                printMatriz xs n 0

            else do 
                putStr (show x)
                printMatriz xs n (v + 1)