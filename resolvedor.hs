module Resolvedor (
  resolve,
  noIntervalo,
  buscaGrupo,
  buscaLinCol
) where

import Matriz
import Data.List (sort)
import Puzzle


resolve :: Int -> String
resolve x =
  if x == 1 then
    "Teste"
  else
    "NOT teste"



-- -=-=-=-=-=-=-=-=- Aqui estão as funções auxiliares -=-=-=-=-=-=-=-=-


-- Busca por elemento x num grupo
buscaGrupo :: Int -> Int -> Bool
buscaGrupo x grupo = x `elem` (Matriz.getGrupo grupo (Matriz.achatarMatriz Puzzle.secundaria))

-- Busca por elemento x na posição [lin][col] da matriz principal
buscaLinCol :: Int -> Int -> Int -> Bool
buscaLinCol x lin col = x `elem` (Puzzle.principal !! lin) || x `elem` (Matriz.getColuna col (n-1))

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