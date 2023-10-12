import Data.Char
{- Histogram -}
number :: Int -> String 
number elem = number' elem ""
    where number' :: Int -> String -> String
          number' elem acc | elem > 0 = number' (elem - 1) $ acc ++ "|"
                           | otherwise = (acc ++ "\n")

makeHistogram' :: (Show a, Eq a) => [a] -> [(a, Int)]
makeHistogram' lst = makeHistogram'' lst (length lst) []
    where makeHistogram'' :: (Show a, Eq a) => [a] -> Int -> [(a, Int)] -> [(a, Int)]
          makeHistogram'' [] len acc = acc
          makeHistogram'' (x:xs) len acc | length xs /= 0 = makeHistogram'' (filter (\y -> y /= x ) (x:xs)) (length (filter (\y -> y /= x) (x:xs))) (acc ++ [(x, (len - (length (filter (\y -> y /= x) (x:xs)))))])
                                         | length xs == 0 = acc ++ [(x, len)]


makeHistogram :: (Show a, Eq a) => [a] -> String
makeHistogram lst = makeHistogram_h (makeHistogram' lst) "Total: " $ length (makeHistogram' lst)
    where makeHistogram_h :: (Show a, Eq a) => [(a, Int)] -> String -> Int -> String
          makeHistogram_h (x:xs) acc len | len == length (x:xs) = makeHistogram_h xs (acc ++ show (len) ++ " elem" ++ "\n \n" ++ show (fst x) ++ " : " ++ number (snd x) ++ "\n") len
                                         | length xs /= 0 = makeHistogram_h xs (acc ++ show (fst x) ++ " : " ++ number (snd x) ++ "\n") len
                                         | otherwise = acc ++ show (fst x) ++ " : " ++ number (snd x)

{- ‘haha almost nice’ -}
sumofsqr :: Int -> Int
sumofsqr n = sumofsqr' (show n) 0
  where sumofsqr' :: String -> Int -> Int
        sumofsqr' [] acc = acc
        sumofsqr' (x:xs) acc = sumofsqr' xs (acc + (digitToInt x) ^ 2)

haha :: Int -> Bool
haha n = haha' n []
    where haha' :: Int -> [Int] -> Bool
          haha' n lst | n == 1 = True
                      | elem n lst = False
                      | otherwise = haha' (sumofsqr n) (lst ++ [n])