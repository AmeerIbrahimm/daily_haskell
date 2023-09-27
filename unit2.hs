
-- lesson 11
half :: Int -> Double
half 0 = error "cant get the half of 0"
half n = fromIntegral n / 2

halve :: Integer -> Integer
halve n = n `div` 2

printDouble :: Int -> String
printDouble x = show (x * 2)

makeAddress :: Int -> (String -> String -> (Int, String, String))
makeAddress doorNo street town = (doorNo, street, town)

same :: a -> a
same x = x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x : xs) = xs

-- lesson 12
type FirstName = String

type LastName = String

type MiddleName = String

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

type Age = Int

type Height = Int

type Weight = Int

patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fname, lname) age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs." ++ show height ++ "in." ++ ")"

type PatientName = (String, String)

firstName :: PatientName -> String
firstName = fst

lastName :: PatientName -> String
lastName = snd

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True -- Universal Donor
canDonateTo _ (BloodType AB _) = True -- Universal reciver
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False -- otherwise

-- data Patient = Patient Name Sex Age Height Weight BloodType

data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

donorFor :: Patient -> Patient -> Bool
donorFor p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

patientSummary :: Patient -> String
patientSummary p =
  "********************\n"
    ++ "Patient: "
    ++ showName (name p)
    ++ "\n"
    ++ "Sex: "
    ++ showSex (sex p)
    ++ "\n"
    ++ "Age: "
    ++ show (age p)
    ++ "\n"
    ++ "Height: "
    ++ show (height p)
    ++ "\n"
    ++ "Weight: "
    ++ show (weight p)
    ++ "\n"
    ++ "Blood: "
    ++ showBloodType (bloodType p)
    ++ "\n"
    ++ "********************"

-- type Classes lesson: 13

myAdd :: (Num a) => a -> a -> a -- here Num is a type class
myAdd x y = x + y

class Discribable a where
  describe :: a -> String

data IceCream = Chocolate | Venilla deriving (Show, Eq, Ord)

cycleSucc n = if n == maxBound
  then minBound
  else succ n


-- lesson 14

data SixSidedDie = S1 | S2 | S3 |S4 | S5 | S6 deriving (Eq , Ord,  Enum)

instance Show SixSidedDie where
  show S1 = "I"
  show S2 = "II"
  show S3 = "III"
  show S4 = "IV"
  show S5 = "V"
  show S6 = "VI"

-- this will cause error if u try [S1 ..]
-- instance Enum SixSidedDie where
--   toEnum 0 = S1
--   toEnum 1 = S2
--   toEnum 2 = S3
--   toEnum 3 = S4
--   toEnum 4 = S5
--   toEnum 5 = S6
--   toEnum _ = error "No such value"

--   fromEnum S1 = 0
--   fromEnum S2 = 1
--   fromEnum S3 = 2
--   fromEnum S4 = 3
--   fromEnum S5 = 4
--   fromEnum S6 = 5

newtype Name1 = Name1 (String, String) deriving (Eq,Show)

instance Ord Name1 where
  compare (Name1 (f1 ,l1)) ( Name1 (f2,l2)) = compare (l2,f2) (l1,f1)


-- Excercise
data Number1 = One | Two | Three

instance Enum Number1 where
  toEnum 1 = One
  toEnum 2 = Two
  toEnum 3 = Three

  fromEnum One = 1
  fromEnum Two = 2
  fromEnum Three = 3

instance Eq Number1 where
  (==) num1 num2 = fromEnum num1 == fromEnum num2

instance Ord Number1 where
  compare num1 num2 = compare (fromEnum num1) (fromEnum num2)

data FiveSidedDie where
  Side1 :: FiveSidedDie
  Side2 :: FiveSidedDie
  Side3 :: FiveSidedDie
  Side4 :: FiveSidedDie
  Side5 :: FiveSidedDie
  deriving (Eq, Enum, Show)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)
