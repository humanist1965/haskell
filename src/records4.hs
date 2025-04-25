{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

class UpdateableRecord a where
  updateRecord :: a -> a -> a

data Person = Person { name :: String, age :: Int } deriving Show
data Car = Car { model :: String, year :: Int } deriving Show

instance UpdateableRecord Person where
  updateRecord updates original =
    original { name = name updates, age = age updates }

instance UpdateableRecord Car where
  updateRecord updates original =
    original { model = model updates, year = year updates }

genericUpdate :: (UpdateableRecord a) => a -> a -> a
genericUpdate = updateRecord

main :: IO ()
main = do
  let person1 = Person { name = "Alice", age = 30 }
  let personUpdates = Person { age = 31 }
  let updatedPerson = genericUpdate personUpdates person1
  print updatedPerson -- Output: Person {name = "Bob", age = 31}

  let car1 = Car { model = "Sedan", year = 2020 }
  let carUpdates = Car { model = "Truck", year = 2023 }
  let updatedCar = genericUpdate carUpdates car1
  print updatedCar -- Output: Car {model = "Truck", year = 2023}