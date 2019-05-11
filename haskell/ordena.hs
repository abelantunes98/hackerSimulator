menor :: (Ord l) => [l] -> l
menor [] = undefined
menor [x] = x
menor (h:t)
 | h <= (menor t) = h
 | otherwise = (menor t)

remove :: (Ord x) => [x] -> x -> [x]
remove [] x = []
remove (h:t) x
 | h == x = t
 | otherwise = [h] ++ (remove t x)

ordena :: (Ord l) => [l] -> [l]
ordena [] = []
ordena l  = [(menor l)] ++ ordena (remove l (menor l))

main :: IO ()
main = do
 let lista = []
 putStrLn $ show (ordena [1,5,8,8,7,6,5,4,3,2,1])

