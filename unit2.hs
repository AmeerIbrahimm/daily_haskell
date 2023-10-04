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

data SixSidedDie = S1 | S2 | S3 |S4 | S5 | S6 deriving (Eq , Ord,  Enum, Bounded)

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

-- lesson 15
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving ( Show , Bounded , Enum )

rotN :: (Enum a, Bounded a) => Int -> a -> a
rotN alphahabetSize c = toEnum rotation
  where halfAlphabet = alphahabetSize `div` 2
        offset = fromEnum c + halfAlphabet
        rotation = offset `mod` alphahabetSize

fourLetterAlphabetEncoder ::  [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
    rot4l = rotN alphaSize


rotNDecoder :: (Enum a, Bounded a) => Int -> a -> a
rotNDecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset = if even halfN
      then fromEnum c + halfN
      else 1 + fromEnum c + halfN
    rotation = offset `mod` n

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotChar text
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotChar = rotNDecoder alphaSize

-- xor based encryption
xorBool :: Bool -> Bool -> Bool
xorBool val1 val2 = (val1 || val2) && not (val1 && val2)

xorPair :: (Bool,Bool) -> Bool
xorPair (val1,val2) = xorBool val1 val2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair $ zip list1 list2

type Bits = [Bool]

intToBits' :: Int -> [Bool]
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
                then False : intToBits' nextVal
                else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse $ intToBits' n
    missingBits = maxBits - length reversedBits
    leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits $ fromEnum char

bitsToInt :: Bits -> Int
bitsToInt bits = sum $ map (\x -> 2 ^ (snd x)) trueLocations
  where size = length bits
        indices = [size-1,size-2..0]
        trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\pair -> (fst pair) `xor` (snd pair)) ( zip padBits plainTextBits)
  where
    padBits = map charToBits pad
    plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bitsList
  where bitsList = applyOTP' pad plainText

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Chipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Chipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

newtype OneTimePad = OTP String

instance Chipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxVal seed = (a * seed + b) `mod` maxVal
