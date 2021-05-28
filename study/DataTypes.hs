module DataTypes where

import           Data.Char (toUpper)

data Task1 = BasicTask1 String Int | ComplexTask1 String Int Location

assignment1 :: Task1
assignment1 = BasicTask1 "Do assignment 1" 60

laundry1 :: Task1
laundry1 = BasicTask1 "Do Laundry" 45

complexTask :: Task1
complexTask = ComplexTask1 "Write Memo" 30 Office

data Location =
  School |
  Office |
  Home

data TaskLength =
  QuarterHour |
  HalfHour |
  ThreeQuarterHour |
  Hour |
  HourAndHalf |
  TwhoHours |
  ThreeHours

data Task2 a =
  BasicTask2 String a |
  ComplexTask2 String a Location

assignment2 :: Task2 Int
assignment2 = BasicTask2 "Do assignment 2" 60

assignment2' :: Task2 TaskLength
assignment2' = BasicTask2 "Do assignment 2" Hour

laundry2 :: Task2 Int
laundry2 = BasicTask2 "Do Laundry" 45

laundry2' :: Task2 TaskLength
laundry2' = BasicTask2 "Do Laundry" ThreeQuarterHour

complexTask2 :: Task2 TaskLength
complexTask2 = ComplexTask2 "Write Memo" HalfHour Office


emptyList :: [Int]
emptyList = []

fullList :: [Int]
fullList = [1, 2, 3]

-- twiceLength :: Task1 -> Int
-- twiceLength (BasicTask1 name time) = 2 * time

capitalizedName :: Task1 -> String
capitalizedName (BasicTask1 name time) = map toUpper name

-- tripleTaskLength :: Task1 -> Task1
-- tripleTaskLength (BasicTask1 name time) = BasicTask1 name (3 * time)

data Task3 = BasicTask3
  { taskName   :: String
  , taskLength :: Int}

twiceLength :: Task3 -> Int
twiceLength task = 2 * taskLength task

assignment3 :: Task3
assignment3 = BasicTask3
  { taskName =  "Do assignment 3"
  , taskLength = 60}

laundry3 :: Task3
laundry3 = BasicTask3
  { taskName = "Do Laundry"
  , taskLength = 45}

tripleTaskLength :: Task3 -> Task3
tripleTaskLength task = task { taskLength = 3 * taskLength task }

-- Error!
-- data Task4 =
--   BasicTask4
--     { taskName4   :: String,
--       taskLength4 :: Int }
--   |
--   ComplexTask4
--     { taskName4     :: String,
--       taskLength4   :: TaskLength, -- Note we use "TaskLength" and not an Int here!
--       taskLocation4 :: Location }
