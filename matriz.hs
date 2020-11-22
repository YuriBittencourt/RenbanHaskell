module Matriz
    (cria_puzzle,
    numGrupos,
    tamGrupos,
    achatarMatriz,
    setPos,
    maximo,
    minimo,
    getGrupo,
    getColuna
    ) where

import Puzzle

--data puzzle = Int Matriz Matriz [[Int]] [Int]
--             n  princ. secun. grupos  tam-grupos


cria_puzzle :: [[Int]] -> Int -> Int
cria_puzzle x n = x!!n!!n

-- Define a quantidade de grupos do puzzle
numGrupos :: [Int] -> Int
numGrupos x = (maximo x) + 1

-- Retorna uma lista com o tamanho de cada grupo
tamGrupos :: [Int] -> [Int] -> [Int]
tamGrupos lista [] = lista
tamGrupos lista matriz = tamGrupos (setPos  (1 + lista!!(head matriz)) (head matriz) lista) (tail matriz)

-- Retorna uma lista contendo o grupo "grupo"
getGrupo :: Int -> [Int] -> [Int]
getGrupo grupo [] = []
getGrupo grupo matriz
    | (grupo == (head matriz)) = (achatarMatriz Puzzle.principal) !! (length (achatarMatriz Puzzle.principal) - length matriz) : getGrupo grupo (tail matriz)
    | otherwise = getGrupo grupo (tail matriz)

-- Inicializa a lista de grupos
setGrupos :: Int -> [[Int]]
setGrupos grupo
    | (grupo < 0) = []
    | otherwise = setGrupos (grupo-1) ++ [getGrupo grupo (achatarMatriz secundaria)]

-- -=-=-=-=-=-=-= Aqui estão as funções auxiliares: -=-=-=-=-=-=-=

-- Define qual o maior valor de uma lista
maximo :: [Int] -> Int
maximo (a:x) = foldr max a x

-- Define qual o menor valor de uma lista
minimo :: [Int] -> Int
minimo (a:x) = foldr min a x

-- Insere x na posição index da lista
setPos :: Int -> Int -> [Int] -> [Int]
setPos x index lista = take index lista ++ [x] ++ drop (index+1) lista

-- Concatena as linhas de uma matriz em uma só lista
achatarMatriz :: [[Int]] -> [Int]
achatarMatriz [] = []
achatarMatriz (a:x) = a ++ concat x

-- Getter para coluna
getColuna :: Int -> [Int] -> [Int]
getColuna st matriz
    | (st >= n*(n-1)) = [matriz !! st]
    | otherwise = [matriz !! st] ++ getColuna (st+n) matriz

-- Getter para linha
getLinha :: Int -> [Int] -> [Int]
getLinha st matriz
    | (st `mod` n == n-1) = [matriz !! st]
    | otherwise = [matriz !! st] ++ getLinha (st+1) matriz

getPos :: [[Int]] -> Int -> Int -> Int
getPos matriz i j = (matriz !! i) !! j