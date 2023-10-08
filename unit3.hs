{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
import Data.Semigroup
import Data.List (sort)
import Foreign.C (e2BIG)
-- lesson: 16

type FirstName = String

type LastName = String

type MiddleName = String

data Book = Book {
        author :: Creator
        , isbn :: String
        , bookTitle :: String
        , bookYear :: String
        , bookPrice :: Double
                 }

data Creator = AuthorCreator Author | ArtistCreator Artist

newtype Author = Author Name

data Artist = Person Name | Band String

data Name = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | FirstNameWithTwoInits FirstName Char Char
  | LastNameWithTwoInits Char Char LastName

authName :: Creator
authName = AuthorCreator (Author (
      LastNameWithTwoInits 'H' 'P' "Lovecraft"
         ))

data VinylRecord = VinylRecord {
        artist :: Creator
        , recordTitle :: String
        , recordYear :: Int
        , recordPrice :: Double
                               }

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectableToy | PamphletItem Pamphlet

data CollectableToy = CollectableToy {
  name :: String
  , toyDescription :: String
  , toyPrice :: Double
  }

data Pamphlet = Pamphlet {
  title :: String
  , phamphletDescription :: String
  , toyContact :: String
                         }

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0

type Radius = Double
type Height = Double
type Width = Double
data Shape = Circle Radius
            | Square Height
            | Rectangle Height Width deriving Show

perimeter :: Shape -> Double
perimeter (Circle r) = 2*pi*r
perimeter (Square h) = 4*h

-- lesson: 17

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

instance Semigroup Integer where
  (<>) x y = x + y

data Color = Red |
  Yellow |
  Green |
  Blue |
  Purple |
  Orange |
  Brown |
  Transparent deriving (Show, Eq)

instance Semigroup Color where
  (<>) any Transparent = any
  (<>) Transparent any = any
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Red Yellow = Orange
  (<>) Yellow Red = Orange
-- to make it type associative we are using gaurds
  (<>) a b | a == b = a
           | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
           | all (`elem` [Blue,Green,Yellow]) [a,b] = Green
           | all (`elem` [Brown,Yellow,Orange]) [a,b] = Orange

instance Monoid Color where
  mempty = Transparent
  mappend col1 col2 = col1 <> col2

-- Monoids

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
    totalProbs = sum probs
    normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|",show prob,"\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where
      pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where
    combiner x y = mconcat [x,"-",y]

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where
          newEvents = combineEvents e1 e2
          newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)
