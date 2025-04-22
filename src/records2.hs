{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

class UpdateField a fieldType where
  updateField :: String -> fieldType -> a -> Maybe a

data Person = Person { name :: String, age :: Int } deriving Show
data Car = Car { model :: String, year :: Int } deriving Show

instance UpdateField Person String where
  updateField "name" newName p = Just $ p { name = newName }
  updateField _ _ _ = Nothing

instance UpdateField Person Int where
  updateField "age" newAge p = Just $ p { age = newAge }
  updateField _ _ _ = Nothing

instance UpdateField Car String where
  updateField "model" newModel c = Just $ c { model = newModel }
  updateField _ _ _ = Nothing

instance UpdateField Car Int where
  updateField "year" newYear c = Just $ c { year = newYear }
  updateField _ _ _ = Nothing

genericUpdateField :: (UpdateField a fieldType) => String -> fieldType -> a -> Maybe a
genericUpdateField fieldName newValue record = updateField fieldName newValue record

main :: IO ()
main = do
  let person1 = Person { name = "Alice", age = 30 }
  let updatedPerson = genericUpdateField "name" "Bob" person1
  print updatedPerson -- Output: Just (Person {name = "Bob", age = 30})

  let car1 = Car { model = "Sedan", year = 2020 }
  let updatedCar = genericUpdateField "year" (2023::Int) car1
  print updatedCar -- Output: Just (Car {model = "Sedan", year = 2023})

  let failedUpdate = genericUpdateField "unknown" (10::Int) person1
  print failedUpdate -- Output: Nothing