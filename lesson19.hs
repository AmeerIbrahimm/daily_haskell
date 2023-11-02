import qualified Data.Map as Map
import qualified Data.List as Lst
import qualified Data.List as Lst

data Organ = Heart | Brain | Kidney | Spleen deriving (Show , Eq)

organs :: [Organ]
organs = [Heart,Spleen,Brain,Spleen,Kidney]

keys :: [Int]
keys = [2,5,54,44,53]

orgPairs :: [(Int,Organ)]
orgPairs = zip keys organs

organCatelog :: Map.Map Int Organ
organCatelog = Map.fromList orgPairs

possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents newkeys organCatelog = map getContentsOrgans newkeys
  where getContentsOrgans x = Map.lookup x organCatelog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatelog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length ( filter (\x -> x == Just organ) available )

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgans :: Maybe Organ -> String
showOrgans (Just organ) = show organ
showOrgans Nothing = ""

organList :: [String]
organList = map showOrgans justTheOrgans

cleanList :: String
cleanList = Lst.intercalate ", " organList

numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just x) = x

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location,Container)
placeInLocation (Vat a) = ( Lab , Vat a )
placeInLocation (Cooler a) = ( Lab , Cooler a )
placeInLocation (Bag a) = ( Kitchen , Bag a )

process :: Organ -> (Location,Container)
process organ = placeInLocation ( organToContainer organ )

report :: (Location,Container) -> String
report (location , container) = show container ++ " in the " ++ show location

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catelog = processAndReport organ
  where organ = Map.lookup id catelog

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report $ process organ
processAndReport Nothing = "error, id not found"

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers organs = length $ filter (== Nothing) organs

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just v) = Just $ f v
maybeMap _ Nothing = Nothing
