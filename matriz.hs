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


-- Define qual o tamanho de cada grupo
tamGrupos :: [Int] -> [Int] -> [Int]
tamGrupos lista [] = lista
tamGrupos lista matriz = tamGrupos (setPos  (1 + (getPos (head matriz) lista)) (head matriz) lista) (tail matriz)



-- Inicializa a lista de grupos
--setGrupos :: []
--setGrupos

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

-- Retorna elemento na posição
getPos :: Int -> [Int] -> Int
getPos x matriz = matriz !! x

-- Insere x na posição n da lista
setPos :: Int -> Int -> [Int] -> [Int]
setPos x n lista = take n lista ++ [x] ++ drop (n+1) lista


-- Concatena as linhas de uma matriz em uma só lista
achatarMatriz :: [[Int]] -> [Int]
achatarMatriz [] = []
achatarMatriz (a:x) = a ++ concat x
