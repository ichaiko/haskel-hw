import Data.Char
import Text.Read (Lexeme(String))

{- 1. Перевод числа в двоичное представление -}
bin :: Int -> [Int]
bin n = binHelper [] n
    where binHelper :: [Int] -> Int -> [Int]
          binHelper lst n|n `div` 2 == 0 = n:lst 
                         |n `mod` 2 == 0 = binHelper (0:lst) (n `div` 2)
                         |n `mod` 2 == 1 = binHelper (1:lst) (n `div` 2)
{- Реализуйте аналог printf(string, [args]) . Он должен парсить входную строку на предмет наличия подстроки %s и подставлять в нее преобразованный в
строку аргумент из args. Напишите сигнатуру для этой функции с помощью Show -}
{- далее закоменчен код, который вроде как должен работать, но нет. Подозреваю, что show должен принимать именно String, a не Char, но не знаю как пофиксить -}
{- showLst :: [a] -> [String]
showLst str = showLstHelper str []
      where showLstHelper :: [a] -> [String] -> [String]
            showLstHelper (x:xs) acc|(length xs == 0) = acc ++ [show x]
                                    |otherwise = showLstHelper xs (acc ++ [show x]) -}

{-printf :: String -> [String] -> String
printf str args = printfHelper str args []
    where printfHelper :: String -> [String] -> String-> String
          printfHelper (x:xs) args acc|(length xs == 0) && (x /= "%s") = acc ++ [x]
                                      |(length xs == 0) && (x == "%s") = acc ++ [head args]
                                      |x == "%s" = printfHelper xs (tail args) (acc ++ [head args])  
                                      |otherwise = printfHelper xs args (acc ++ [x])-}

{- 4. В последовательности записаны целые числа от 1 до N в произвольном
порядке, но одно из чисел пропущено (остальные встречаются ровно по одному разу). N заранее неизвестно. Определить пропущенное число -}
sortList :: [Int] -> [Int] {- уже знаю как реализовать умнее, но у меня уже была написана такая сортировка + она подходит под условие -}
sortList n = sortListHelper [] n
      where sortListHelper :: [Int] -> [Int] -> [Int]
            sortListHelper acc n|n == [] = acc
                                |otherwise = sortListHelper (acc ++ [minimum n]) (filter (\x -> if x /= (minimum n) then True else False) n)

missedNumber :: [Int] -> Int
missedNumber n = missedNumberHelper (sortList n)
    where missedNumberHelper :: [Int] -> Int
          missedNumberHelper (x:xs)|(x + 1) == head xs =  missedNumberHelper xs
                                   |otherwise = x + 1

{- 3. Реализуйте функцию, которая на вход получает строку из цифр, а на выходе
отдает число, которое распарсила -}
parsing :: String -> Int
parsing str = parsingHelper (arrayOfNumbers str []) (length str - 1) 0
      where parsingHelper :: [Int] -> Int -> Int -> Int
            parsingHelper [] _ acc = acc
            parsingHelper (x:xs) len acc = parsingHelper xs (len - 1) (acc + x * (10 ^ len))

            arrayOfNumbers :: String -> [Int] -> [Int]
            arrayOfNumbers [] acc = acc
            arrayOfNumbers (x:xs) acc | x == '0' = arrayOfNumbers xs (acc ++ [0])
                                      | x == '1' = arrayOfNumbers xs (acc ++ [1])
                                      | x == '2' = arrayOfNumbers xs (acc ++ [2])
                                      | x == '3' = arrayOfNumbers xs (acc ++ [3])
                                      | x == '4' = arrayOfNumbers xs (acc ++ [4])
                                      | x == '5' = arrayOfNumbers xs (acc ++ [5])
                                      | x == '6' = arrayOfNumbers xs (acc ++ [6])
                                      | x == '7' = arrayOfNumbers xs (acc ++ [7])
                                      | x == '8' = arrayOfNumbers xs (acc ++ [8])
                                      | x == '9' = arrayOfNumbers xs (acc ++ [9])
{- 2. Перевод числа из основания n в 10чную, с помощью схемы горнера сверткой -}
convertTo10 :: String -> Int -> Int
convertTo10 str n = foldl (\res new -> res * n + new) 0 (convertToInt str [] ['A'..'Z'])
      where convertToInt :: String -> [Int] -> [Char] -> [Int]
            convertToInt [] acc letters = acc 
            convertToInt (x:xs) acc letters|elem x letters == True = convertToInt xs (acc ++ [ord x - 55]) letters
                                           |otherwise = convertToInt xs (acc ++ [ord x - 48]) letters
{- Реализуйте аналог printf(string, [args]) . Он должен парсить входную строку на предмет наличия подстроки %s и подставлять в нее преобразованный в
строку аргумент из args. Напишите сигнатуру для этой функции с помощью Show -}
printf :: String -> [String] -> String
printf str args = printfHelper str args []
      where printfHelper :: String -> [String] -> String -> String
            printfHelper (x:xs) args acc| length args == 0 = acc ++ ([x] ++ xs)
                                        | x /= '%' = printfHelper xs args (acc ++ [x])
                                        | (x == '%') && (length xs == 1) = acc ++ head args
                                        | (x == '%') && (length xs > 0) = printfHelper (tail xs) (tail args) (acc ++ head args)