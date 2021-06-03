module MyFirstModule where

myFirstExpression :: String
myFirstExpression = "Hello World!"

myFirstFunction :: String -> String
myFirstFunction input = "Hello " ++ input ++ "!"

myIfStatement :: Int -> Int
myIfStatement a =
  if a <= 2
    then a + 2
    else a - 2

myGuardStatement :: Int -> Int
myGuardStatement a
  | a <= 2 = a + 2
  | a <= 6 = a
  | otherwise = a - 2

myPatternFunction  :: [Int] -> Int
myPatternFunction [a]         = a + 3
myPatternFunction [a,b]       = a + b + 1
myPatternFunction (1 : 2: _)  = 3
myPatternFunction (3 : 4 : _) = 7
myPatternFunction xs          = length xs


mathFunction :: Int -> Int -> Int -> Int
mathFunction a b c = (c - a) + (b - a) + (a * b * c) + a


mathFunctionWhere :: Int -> Int -> Int -> Int
mathFunctionWhere a b c = diff1 + diff2 + prod + a
  where
    diff1 = c - a
    diff2 = b - 1
    prod = a * b * c

mathFunctionLet :: Int -> Int -> Int -> Int
mathFunctionLet a b c =
  let diff1 = c - a
      diff2 = b - a
      prod = a * b * c
  in diff1 + diff2 + prod + a
