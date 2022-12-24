{- Задача 1. Реализовать функцию (!!), считающую элементы от 0. -}
indexFinder :: [a] -> Int -> a
indexFinder [] ind = error "empty list"
indexFinder lst ind = indexFinderHelper lst ind 0
    where indexFinderHelper :: [a] -> Int -> Int -> a
          indexFinderHelper (x:xs) ind acc|acc == ind = x
                                          |acc /= ind = indexFinderHelper xs ind (acc + 1)
                                          |(length xs == 0) && (acc /= ind) = error "index out of range" 

{- Задача 2. Используя рекурсию по списку, написать функцию init, -}
init' :: [a] -> [a]
init' (x:xs) = if length xs /= 0 then [x] ++ init' xs else n
    where n = []
init' [] = error "error"

{- Задача 3. Конкатенация двух списков -}
concatenation :: [a] -> [a] -> [a]
concatenation lst [] = lst
concatenation lst (x:xs) = concatenation (lst ++ [x]) xs

{- Задача 4. Бесконечный циклический повтор конечных списков -}
cycle :: [a] -> [a]
cycle lst = cycleHelper lst lst
    where cycleHelper :: [a] -> [a] -> [a]
          cycleHelper lst lst1 = cycleHelper (lst ++ lst1) lst1

{- Задача 5. Подсписок первых n элементов, начиная с нулевого -}
take' :: Int -> [a] -> [a]
take' amount lst = takeHelper amount lst 1 []
    where takeHelper :: Int -> [a] -> Int -> [a] -> [a]
          takeHelper amount (x:xs) cur acc|cur > amount = error "index out of range"
                                          |cur /= amount = takeHelper amount xs (cur + 1) (acc ++ [x])
                                          |cur == amount = acc ++ [x]
                                          
{- Задача 6. Список всех начальных сегментов inits и конечных сегментов tails [1, 2, 3] -> [[],[1],[1,2],[1,2,3]]; [1, 2, 3] -> [[1,2,3],[2,3],[3],[]]-}
inits' :: [a] -> [[a]]
inits' lst = initHelper lst [] []
    where initHelper :: [a] -> [a] -> [[a]] -> [[a]]
          initHelper [] acc lst = lst ++ [acc]
          initHelper (x:xs) acc lst|length xs /= 0 = initHelper xs (acc ++ [x]) (lst ++ [acc])
                                   |length xs == 0 = initHelper xs (acc ++ [x]) (lst ++ [acc])

tails' :: [a] -> [[a]]
tails' lst = tailHelper lst [] lst
    where tailHelper :: [a] -> [[a]] -> [a] -> [[a]]
          tailHelper [] lst lst2 = lst2:lst
          tailHelper (x:xs) lst lst2|length xs /= 0 = tailHelper xs (lst ++ [xs]) lst2
                                    |length xs == 0 = tailHelper xs (lst ++ [[x]]) lst2

{- Задача 7. Написать функцию‐предикат, проверяющую — входит ли данный эле‐
мент в данный конечный список. -}
elem' :: Eq a => a -> [a] -> Bool
elem' n (x:xs) | n == x              = True
               | length xs == 0      = False
               | elem' n xs == True  = True
               | otherwise           = False

{- Задача 8. Функция nub, возвращает список, в котором удалены повторяющиеся
элементы. -}
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' lst = nubHelper lst []
    where nubHelper :: Eq a => [a] -> [a] -> [a]
          nubHelper [] acc = acc
          nubHelper (x:xs) acc|elem x acc == False = nubHelper xs (acc ++ [x])
                              |elem x acc == True = nubHelper xs acc
                              
{- Задача 9. Написать функцию updElmBy которая для данного списка и индекса из‐
меняет значение на заданное и возвращает новый список. -}
updElmBy :: [a] -> Int -> a -> [a]
updElmBy lst ind el = updElmByHelper lst ind el 0 []
    where updElmByHelper :: [a] -> Int -> a -> Int -> [a] -> [a]
          updElmByHelper [] ind el acc fl = fl 
          updElmByHelper (x:xs) ind el acc fl|ind /= acc = updElmByHelper xs ind el (acc + 1) (fl ++ [x])
                                             |ind == acc = updElmByHelper xs ind el (acc + 1) (fl ++ [el])

{- Задача 10. Написать функцию swp которая для данного списка и индексов i,j вза‐
имно меняет их значения и возвращает новый список. -}
swp :: [a] -> Int -> Int -> [a]
swp lst i j = swpHelper lst i j (indexFinder lst i 0) (indexFinder lst j 0)  0 [] 
    where swpHelper :: [a] -> Int -> Int -> a -> a -> Int -> [a] -> [a]
          swpHelper [] i j vi vj ind acc = acc
          swpHelper (x:xs) i j vi vj ind acc|(ind /= i) && (ind /= j) = swpHelper xs i j vi vj (ind + 1) (acc ++ [x])
                                            |ind == i = swpHelper xs i j vi vj (ind + 1) (acc ++ [vj])
                                            |ind == j = swpHelper xs i j vi vj (ind + 1) (acc ++ [vi])
        
          indexFinder :: [a] -> Int -> Int -> a
          indexFinder (x:xs) i acc|acc == i = x
                                  |acc /= i = indexFinder xs i (acc + 1)

{-Задача 11. Функция перестановок permutations для данного конечного списка
возвращает список всех возможных перестановок-}
perms :: [a] -> [[a]]
perms = perms' 0
  where perms' _ [x, y] = [[x, y], [y, x]]
        perms' c xs | c == length xs = []
                    | otherwise = (sub_perm xs) ++ (perms' (c + 1) (shift xs))
        sub_perm (x:xs) = fmap (\a -> x:a) $ perms xs
        shift xs = (last xs):(init xs)

{- Задача 12. Функция subsequences для данного конечного списка возвращает спи‐
сок всех возможных подпоследовательностей . -}
subsequences' :: [a] -> [[a]]
subsequences' lst = [[]] ++ foldr(\acc curr -> [acc] : add acc curr ++ curr) [] lst
    where add x lst = foldl (\acc curr -> acc ++ [x:curr]) [] lst

{- Задача 13. Дан конечный произвольный список целых чисел. Используя свертку
foldr вычислить сумму кубов этих чисел. Другими словами, написать функциюcubsum,
которая -}
cubR :: [Int] -> Int
cubR lst = foldr(\acc curr -> curr + acc^3) 0 lst

{- Задача 14. Эту же самую задачу решить, используя вместо foldr свертку foldl -}
cubL :: [Int] -> Int
cubL lst = foldl(\acc curr -> acc + curr^3) 0 lst

{- Задача 16. Посчитать, сколько раз в данном конечном списке содержится данное
значение? -}
howmany :: Eq a => a -> [a] -> Int
howmany x lst = foldl(\acc curr -> if curr == x then acc + 1 else acc) 0 lst