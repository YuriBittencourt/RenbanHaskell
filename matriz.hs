module Matriz
  (cria_puzzle,
  numGrupos,
  tamGrupos,
  achatarMatriz
  ) where

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
tamGrupos lista matriz = tamGrupos (setPos  (1 + (getPos (head matriz) lista)) (head matriz) lista) (tail matriz)

-- Retorna uma lista contendo o grupo "grupo"
getGrupo :: Int -> [Int] -> [Int]
getGrupo grupo [] = []
getGrupo grupo matriz
    | (grupo == (head matriz)) = (achatarMatriz principal) !! (length (achatarMatriz principal) - length matriz) : getGrupo grupo (tail matriz)
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

-- Conta a quantidade de ocorrências de um número em uma lista
ocorrencias :: [Int] -> Int -> Int
ocorrencias (a:b) c =
  if (a == c) then
    1 + (ocorrencias b c)
  else
    (ocorrencias b c)

-- Insere x na posição index da lista
setPos :: Int -> Int -> [Int] -> [Int]
setPos x index lista = take index lista ++ [x] ++ drop (index+1) lista


-- Concatena as linhas de uma matriz em uma só lista
achatarMatriz :: [[Int]] -> [Int]
achatarMatriz [] = []
achatarMatriz (a:x) = a ++ concat x
