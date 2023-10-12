-- task 1 ' quadratic equation '
-- Solve quadratic equation
-- In case it has no roots, return Nothing
quadraticSolver :: Double -> Double -> Double -> Maybe (Double, Double)
quadraticSolver a b c | (b^2 - 4 * a * c) < 0 = Nothing
                      | otherwise = Just ((-b + (b^2 - 4 * a * c)**0.5) / (2 * a), (-b - (b^2 - 4 * a * c)**0.5) / (2 * a))

-- task 2 ' maybe lists stdlib '
-- Implement the following lists functions using Maybe data structure
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (x:xs) = Just xs

maybeInit :: [a] -> Maybe [a]
maybeInit [] = Nothing
maybeInit lst = maybeInitHelper lst []
    where maybeInitHelper :: [a] -> [a] -> Maybe [a]
          maybeInitHelper (x:xs) acc | length xs /= 0 = maybeInitHelper xs (acc ++ [x])
                                     | otherwise = Just acc

maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind _ [] = Nothing
maybeFind predicate (x:xs) | (length xs == 0) && (predicate x == False) = Nothing
                           | (length xs == 0) && (predicate x == True) = Just x
                           | predicate x == True = Just x
                           | predicate x == False = maybeFind predicate xs

-- task 3 ' pattern matching with data structures '
-- implement undefined functions

data DogBreed = GoldenRetrievers
              | BostonTerriers
              | LabradorRetrievers
              | Poodles
              | BorderCollie
              | Beagle
              | IrishSetter
              | Staffordshire
              | Bull
              | Terrier
    deriving (Show, Eq)

data Gender = Male | Female
    deriving (Show, Eq)

data Dog = Dog { name :: String
               , age :: Int
               , gender :: Gender
               , breed :: DogBreed
               , isGoodBoy :: Bool -- holds for female dogs as well
               } deriving (Show, Eq)

dogs = [ Dog "Leander" 12 Male Beagle False
       , Dog "Ouranos" 1 Male Poodles True
       , Dog "Pegasus" 2 Female Beagle False
       , Dog "Atlas" 8 Female GoldenRetrievers True
       , Dog "Castor" 6 Male LabradorRetrievers True
       , Dog "Apollo" 3 Female Beagle False
       , Dog "Narkissos" 15 Male Beagle True
       , Dog "Dardanos" 7 Female Terrier True
       , Dog "Ajax" 4 Male IrishSetter False
       , Dog "Pyrrhos" 2 Female BorderCollie False
       , Dog "Patroclus" 6 Male Bull True
       , Dog "Iacchus" 4 Female Beagle True ]

-- dogs which are good boys
goodBoys :: [Dog]
goodBoys = filter (\x -> if goodBoys' x == True then True else False) dogs
    where goodBoys' :: Dog -> Bool
          goodBoys' (Dog _ _ _ _ True) = True
          goodBoys' (Dog _ _ _ _ False) = False

-- dogs with name longer than 7 symbols
longNamedDogs :: [Dog]
longNamedDogs = filter (\x -> length (name x) > 7) dogs

-- among dogs, which is the most popular gender?
mostPopularDogGender :: Gender
mostPopularDogGender = if length (filter (\x -> gender x == Male) dogs) >= 6 then Male else Female

oldestDog :: Dog
oldestDog = oldestDog' dogs (Dog "Leander" 12 Male Beagle False)
    where oldestDog' :: [Dog] -> Dog -> Dog
          oldestDog' (x:xs) acc | (length xs /= 0) && (age x >= age (head xs)) = oldestDog' (tail xs) x
                                | (length xs /= 0) && (age x < age (head xs)) = oldestDog' (tail xs) (head xs)
                                | otherwise = acc

averageDogAge :: Double
averageDogAge = (averageDogAge' dogs 0) / 12
    where averageDogAge' :: [Dog] -> Double -> Double
          averageDogAge' (x:xs) acc | length xs /= 0 = averageDogAge' xs (acc + fromIntegral (age x))
                                    | otherwise = acc + fromIntegral (age x)

dogsByBreed :: DogBreed -> [Dog]
dogsByBreed breed' = dogsByBreed' breed' dogs []
    where dogsByBreed' :: DogBreed -> [Dog] -> [Dog] -> [Dog]
          dogsByBreed' breed' (x:xs) acc | (length xs > 0) && ((breed x) == breed') = dogsByBreed' breed' xs (acc ++ [x])
                                         | (length xs > 0) && ((breed x) /= breed') = dogsByBreed' breed' xs acc
                                         | (length xs == 0) && ((breed x) == breed') = acc ++ [x]
                                         | otherwise = acc

-- task 4.1

-- Создайте тип, который реализует комплексные числа
-- создайте функции, которые реализуют:
-- - сумму, разницу
-- - умножение, деление
-- - взятие сопряженного
-- - взятие абсолютного значения

data Comp = Comp{ real :: Double,
                  image :: Double
                } deriving (Show)

sum :: Comp -> Comp -> Comp
sum x y = Comp (real x + real y) (image x + image y)

sub :: Comp -> Comp -> Comp
sub x y = Comp (real x - real y) (image x - image y)

mult :: Comp -> Comp -> Comp
mult x y = Comp (real x * real y - image x * image y) (real x * image y + image x * real y)

div :: Comp -> Comp -> Comp
div x y = Comp ((real x * real y + image x * image y) / ((real y) ** 2 + (image y) ** 2)) ((real y * image x - real x * image y) / ((real y) ** 2 + (image y) ** 2))

conjuagate :: Comp -> Comp
conjuagate x = Comp (real x) (-1 * image x)

abs :: Comp -> Comp
abs x = Comp (real x * 2) (image x * 2)

-- Создайте тип, который образует односвязный список (<=> список имеет голову и хвост, либо является пустым)
-- реализуйте  для него следующие методы:

data MyList a = EmptyList | MyList a (MyList a)
    deriving(Show)

fromList :: [a] -> MyList a
fromList [] = EmptyList
fromList lst = MyList (head lst) (fromList $ tail lst)

toList :: MyList a -> [a]
toList EmptyList = []
toList (MyList x n) = (x:(toList n))

reverseMyList :: MyList a -> MyList a
reverseMyList EmptyList = EmptyList
reverseMyList lst = reverseMyList' lst EmptyList
    where reverseMyList' :: MyList a -> MyList a -> MyList a
          reverseMyList' EmptyList acc = acc
          reverseMyList' (MyList x xs) acc = reverseMyList' xs (MyList x acc)

-- should do the same thing as standard map
mapMyList :: (a -> b) -> MyList a -> MyList b
mapMyList x EmptyList = EmptyList 