split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

leMatriz :: Int -> [[Int]] -> [[Int]]
leMatriz 0 lst = lst
leMatriz i lst = do
            linhaIO <- getLine
            let linha = read linhaIO :: [Char]
            leMatriz (i-1) (lst ++ (split ' ' linha))

leArquivo :: (Int, [[Int]], [[Int]])
leArquivo = do
    file <- getContents
    let n = (read file :: String)
    _ <- getLine

    let matrizPrincipal = leMatriz n []

    _ <- getLine

    let matrizSecundaria = leMatriz n []

    return (n, matrizPrincipal, matrizSecundaria)



main = do
    print leArquivo()