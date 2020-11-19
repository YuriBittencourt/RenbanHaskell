module Resolvedor (resolve) where

resolve :: Int -> String
resolve x =
  if x == 1 then
    "Teste"
  else
    "NOT teste"
