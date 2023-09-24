import Text.XHtml.Transitional (abbr)

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
