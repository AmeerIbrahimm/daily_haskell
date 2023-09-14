import Distribution.Compat.CharParsing (integral)

main :: IO ()
main = do
  print "who is this email for?"
  recipient <- getLine
  print "What is the title"
  title <- getLine
  print "Who is the Author"
  author <- getLine
  print $ createEmail recipient title author

toPart :: String -> String
toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart :: String -> String
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"

fromPart :: String -> String
fromPart author = "Thanks, \n" ++ author

createEmail :: String -> String -> String -> String
createEmail recipient bookTitle author = toPart recipient ++ bodyPart bookTitle ++ fromPart author

inc :: (Num a) => a -> a
inc x = x + 1

double' :: (Num a) => a -> a
double' x = x * 2

square :: (Num a) => a -> a
square x = x * x

sumSquaredOrsquaredSum x y =
  let sumSquared = (x ^ 2 + y ^ 2)
      squaredSum = (x + y) ^ 2
   in if sumSquared > squaredSum
        then sumSquared
        else squaredSum
