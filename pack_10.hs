import Barans
import Data.Maybe
import Control.Monad (guard)


find_grandFather :: Sheep -> Maybe Sheep
find_grandFather sheep = case mother sheep of
    Just m -> father m
    Nothing -> Nothing

greatgrandFather sheep = find_grandFather sheep >>= father

listOfParents sheep | mother sheep == Nothing && father sheep == Nothing = []
                    | mother sheep == Nothing = [fromJust $ father sheep]
                    | father sheep == Nothing = [fromJust $ mother sheep]
                    | otherwise = [fromJust $ mother sheep] ++ [fromJust $ father sheep]

listOfGrandParents :: Sheep -> [Sheep]
listOfGrandParents sheep = concatMap listOfParents (listOfParents sheep)

isParentless :: Sheep -> Bool
isParentless sheep = (length $ listOfParents sheep) == 0

selected_barans :: [Sheep]
selected_barans = ["i3", "i5", "i6", "i9", "i12"]

selectedFather :: Sheep -> Maybe Sheep
selectedFather sheep = do
    if fatherExists && isFatherSelected then Just (fromJust $ father sheep)
    else Nothing
    where
        fatherExists = father sheep /= Nothing
        isFatherSelected = elem (fromJust $ father sheep) selected_barans /= False

closestFathersRelative :: Sheep -> Maybe Sheep
closestFathersRelative sheep | father sheep == Nothing = Nothing
                             | elem (fromJust $ father sheep) selected_barans == True = father sheep
                             | otherwise = closestFathersRelative $ fromJust $ father sheep