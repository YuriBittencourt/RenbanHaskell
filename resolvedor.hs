--module Resolvedor (resolve) where

import Matriz
import Data.List (sort)
import System.Exit

resolve :: Int -> String
resolve x =
  if x == 1 then
    "Teste"
  else
    "NOT teste"



-- -=-=-=-=-=-=-=-=- Aqui estão as funções auxiliares -=-=-=-=-=-=-=-=-


-- Confere se existe uma sequência de números em uma lista
ehSequencia :: [Int] -> Bool
ehSequencia [] = True
ehSequencia lista =
  if (sort lista) == [((sort lista)!!0)..(last(sort lista))] then
    True
  else
    False



-- Main para testes

main = do
  print(ehSequencia   [1, 3, 2, 4])
  print(ehSequencia [1, 2, 5, 6])
