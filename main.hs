import Matriz
import Resolvedor

n :: Int
n = 7

principal :: [[Int]]
principal = [[0, 0, 7, 0, 5, 0, 2],
            [0, 0, 0, 0, 0, 0, 0],
            [2, 0, 0, 0, 0, 1, 0],
            [1, 0, 5, 2, 0, 0, 0],
            [0, 5, 0, 0, 0, 0, 0],
            [0, 0, 1, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 6, 4]]

secundaria :: [[Int]]
secundaria = [[0, 1, 2, 2, 3, 4, 4],
            [18, 18, 19, 2, 3, 4, 5],
            [16, 17, 20, 20, 3, 6, 7],
            [16, 16, 20, 3, 3, 7, 7],
            [14, 15, 15, 15, 15, 8, 8],
            [13, 12, 12, 11, 11, 9, 9],
            [13, 12, 12, 11, 10, 9, 9]]

main = do

--print(Matriz.numGrupos [[1,2,3,5,4], [8,6]])
--print(Matriz.cria_puzzle [[1],[2]] 0)
--print(Resolvedor.resolve 1)

  let matrizLista = Matriz.achatarMatriz (secundaria)
  print (Matriz.numGrupos (matrizLista))
  print (Matriz.tamGrupos (replicate (Matriz.numGrupos matrizLista) 0) matrizLista)
