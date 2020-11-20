import Puzzle
import Matriz
import Resolvedor

main = do

  let matrizLista = Matriz.achatarMatriz (Puzzle.secundaria)
  --print (Matriz.numGrupos (matrizLista))
  --print (Matriz.tamGrupos (replicate (Matriz.numGrupos matrizLista) 0) matrizLista)
  print(Resolvedor.buscaGrupo 7 2)
  print(Resolvedor.buscaLinCol 4 1)
  print(Resolvedor.noIntervalo [0,0,0,8] 4 1)
