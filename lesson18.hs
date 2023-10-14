import Data.Map (Map)
import qualified Data.Map as Map
data Box a = Box a deriving Show

data Triple a = Triple a a a deriving Show

type Names = Triple String

type Point3D = Triple Double

name :: Names
name = Triple "Ameer" "ibrahim" "m"

coordinate :: Point3D
coordinate = Triple 0.1 0.12 0.5

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z)= Triple (f x) (f y) (f z)

--Defining own list
data List a = Empty | Cons a (List a) deriving Show

builtInListEx1 :: [Int]
builtInListEx1 = 1:2:3:[]

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtInEx2 :: [Char]
builtInEx2 = 'c':'a':'r':[]

ourListEx2 :: List Char
ourListEx2 = Cons 'c' (Cons 'a' (Cons 'r' Empty))

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap f (Cons a rest) = Cons (f a) (ourMap f rest)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box a) = Box (f a)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

data Organ = Heart | Brain | Kidney | Spleen deriving (Show,Eq, Ord)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

keys :: [Int]
keys = [1,4,5,7,9,15]

organPairs :: [(Int,Organ)]
organPairs = zip keys organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

organCate :: Map.Map Organ Int
organCate = Map.fromList $ zip organs keys
