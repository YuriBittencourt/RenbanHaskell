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

-- Busca por elemento x na linha e coluna de índice "index"
buscaLinCol :: Int -> Int -> Bool
buscaLinCol x index = x `elem` (Puzzle.principal !! index) || x `elem` (Matriz.getColuna index (n-1))

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
