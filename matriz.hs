module Matriz (cria_puzzle, def_numGrupos) where

--data puzzle = Int Matriz Matriz [[Int]] [Int]
--             n  princ. secun. grupos  tam-grupos


cria_puzzle :: [[Int]] -> Int -> Int
cria_puzzle x n = x!!n!!n

-- Define a quantidade de grupos do puzzle
def_numGrupos :: [[Int]] -> Int
def_numGrupos x = maximo (concatenar_matriz x)


-- Define qual o tamanho de cada grupo
--def_tamGrupos :: [[Int]] -> [Int]
--def_tamGrupos secun =


-- Inicializa a lista de grupos
--setGrupos :: []
--setGrupos

-- -=-=-=-=-=-=-= Aqui estão as funções auxiliares: -=-=-=-=-=-=-=

-- Define qual o maior valor de uma lista
maximo :: [Int] -> Int
maximo (a:x) = foldr max a x

-- Concatena as linhas de uma matriz em uma só lista
concatenar_matriz :: [[Int]] -> [Int]
concatenar_matriz [] = []
concatenar_matriz (a:x) = a ++ concat x
