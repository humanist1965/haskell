{-# LANGUAGE DeriveDataTypeable #-}
import Data.Data
-- Note: We no longer need Data.Text
-- import Data.Text (Text, pack)

data Person = Person { name :: String, age :: Int } deriving (Show, Data, Typeable)
data Car = Car { model :: String, year :: Int } deriving (Show, Data, Typeable)

getFieldNames :: (Data a) => a -> [String]
getFieldNames x =
  let constructor = toConstr x
  in case constrFields constructor of
    [] -> [] -- Not a record constructor or no fields
    fields -> fields -- Field names are already Strings

main :: IO ()
main = do
  let person = Person { name = "Alice", age = 30 }
  let car = Car { model = "Sedan", year = 2020 }

  print (getFieldNames person)
  print (getFieldNames car)