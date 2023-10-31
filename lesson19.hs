import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show , Eq)

organs :: [Organ]
organs = [Heart,Spleen,Brain,Spleen,Kidney]

keys :: [Int]
keys = [2,5,54,44,53]

orgPairs :: [(Int,Organ)]
orgPairs = zip keys organs

organCatelog :: Map.Map Int Organ
organCatelog = Map.fromList orgPairs

getOrgans :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getOrgans newkeys organCatelog = map getContentsOrgans newkeys
  where getContentsOrgans x = Map.lookup x organCatelog
