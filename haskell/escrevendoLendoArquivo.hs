-criando arquivo txt e lendo-

main :: IO()
main = do
    x <- getLine
    writeFile "teste2.txt" (x)
    y <- readFile "teste2.txt"
    putStrLn $ y