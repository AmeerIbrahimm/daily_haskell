import Control.Applicative (Alternative (empty))
import Data.Binary.Get (isEmpty)

comparaLastNames name1 name2 =
  if comp == EQ
    then comparaFirstNames name1 name2
    else comp
  where
    comp = compare lastName1 lastName2
    lastName1 = snd name1
    lastName2 = snd name2

comparaFirstNames name1 name2 =
  compare firstName1 firstName2
  where
    firstName1 = fst name1
    firstName2 = fst name2

sfOffice name =
  if lastName > "L"
    then fullName ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else fullName ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    fullName = fst name ++ " " ++ snd name
    lastName = snd name

nyOffice name = fullName ++ ": PO Box 789 - New York, NY, 10013"
  where
    fullName = fst name ++ " " ++ snd name

renoOffice name = lastName ++ " - PO Box 456 - Reno, NV 89523"
  where
    lastName = snd name

dcOffice name = fullName ++ " Esq"
  where
    fullName = fst name ++ " " ++ snd name

getLocation location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> dcOffice
  _ -> (\name -> fst name ++ " " ++ snd name)

addressLetter name location = locationFunc name
  where
    locationFunc = getLocation location

ifEven f x = if even x then f x else x

genIfEven f = (\x -> f x)

genIfXEven x = (\f -> f x)

getRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

getHostRequestBuilder host = (\apiKey resource id -> getRequestUrl host apiKey resource id)

firstHalfElem x xs =
  x `elem` half || False
  where
    half = take ((length xs) `div` 2) xs

subSeq startind endind xs = drop startind $ take endind xs

gcd' a b =
  if a `mod` b == 0
    then b
    else gcd' b $ a `mod` b

sayNumber :: Int -> String
sayNumber 1 = "one"
sayNumber 2 = "two"
sayNumber 3 = "three"
sayNumber _ = "other"

isEmpty' :: [a] -> Bool
isEmpty' [] = True
isEmpty' _ = False

myHead [] = error "that was an empty list you moron!"
myHead (x : _) = x

myTail [] = []
myTail (_ : xs) = xs

myGhd _ 1 = 1
myGhd x 0 = x
myGhd x y = myGhd y (x `mod` y)

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x : xs) = x : rest
  where
    rest = myTake (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop 1 (_ : xs) = xs
myDrop n (_ : xs) = myDrop (n - 1) xs

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz 1 = 1
collatz n =
  if even n
    then 1 + collatz (n `div` 2)
    else 1 + collatz (n * 3 + 1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x : xs) =
  if f x
    then x : myFilter f xs
    else myFilter f xs

remove' :: (a -> Bool) -> [a] -> [a]
remove' _ [] = []
remove' f (x : xs) =
  if f x
    then remove' f xs
    else x : remove' f xs

myProduct [] = error "that is an empty list"
myProduct xs = foldl (*) 1 xs

concatAll [] = ""
concatAll xs = foldl (++) "" xs
