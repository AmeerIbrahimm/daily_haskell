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
