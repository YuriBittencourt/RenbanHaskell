module Matriz
    (cria_puzzle,
    numGrupos,
    tamGrupos,
    achatarMatriz,
    setPos,
    getPos
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

-- Busca por elemento x num grupo
buscaGrupo :: Int -> Int -> Bool
buscaGrupo x grupo = x `elem` (getGrupo grupo (achatarMatriz secundaria))

-- Busca por elemento x na linha e coluna de índice "index"
buscaLinCol :: Int -> Int -> Bool
buscaLinCol x index = x `elem` (principal !! index) || x `elem` (getColuna index (n-1))

-- Verifica se um numero está no intervalo possivel do grupo
noIntervalo :: [Int] -> Int -> Int -> Bool
noIntervalo grupo tamanho valor
    |  grupo == [] = True
    |  (length grupo) - 1 == tamanho = ehSequencia (grupo:valor)
    |  otherwise = (valor >= ((minimo grupo) - (tamanho - (((maximo grupo) - (minimo grupo) + 1))))) && (valor <= ((maximo grupo) - (tamanho - (((maximo grupo) - (minimo grupo) + 1)))))
-- -=-=-=-=-=-=-= Aqui estão as funções auxiliares: -=-=-=-=-=-=-=

-- Define qual o maior valor de uma lista
maximo :: [Int] -> Int
maximo (a:x) = foldr max a x

-- Insere x na posição index da lista
setPos :: Int -> Int -> [Int] -> [Int]
setPos x index lista = take index lista ++ [x] ++ drop (index+1) lista

-- Concatena as linhas de uma matriz em uma só lista
achatarMatriz :: [[Int]] -> [Int]
achatarMatriz [] = []
achatarMatriz (a:x) = a ++ concat x

-- Getter para coluna
getColuna :: Int -> Int -> [Int]
getColuna col (-1) = []
getColuna col index = getColuna col (index-1) ++ [(principal !! index) !! col]
