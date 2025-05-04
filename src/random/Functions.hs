{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
-- | This is a polymorphic function that takes two arguments and returns their sum
-- | It works on any type that is an instance of the Num typeclass
test1 :: Num a => a -> a -> a
test1 x y = x + y

-- | This function is a partial application of test1 and shows how currying works
test2 :: Num a => a -> a
test2 = test1 1000

-- | This function is used to show pattern matching
test3 :: [Int]-> [Int]

-- | There are 4 cases in this function
test3 [] = []
test3 (1:xs) = 11111 : test3 xs
test3 (3:xs) = 33333 : test3 xs
test3 (x:xs) = x*2 : test3 xs

-- | Besides pattern matching, You can also use guards
test4 [] = []
test4 (1:xs) = 11111 : test4 xs
test4 (3:xs) = 33333 : test4 xs

test4 (x:xs)
    | x > 10 && x < 20  = 10 : test4 xs
    | x > 20 && x < 30  = 30 : test4 xs
    | otherwise         = x*2 : test4 xs

test5 x = f x where
    f [] = []
    f (1:xs) = 11111 : f xs
    f (3:xs) = 33333 : f xs

    f (x:xs)
        | x > 10 && x < 20  = 10 : f xs
        | x > 20 && x < 30  = 30 : f xs
        | otherwise         = x*2 : f xs

-- | First look at a lambda function
test6 :: Integer -> Integer -> Integer
test6 = (\x y -> x + y) 

test7 :: [Int] -> [Int]
test7 xs = map (\x -> x*2) xs


-- This shows currying more clearly
test8 :: Num a => a -> a -> a
test8 = (\x -> (\y -> x + y))


