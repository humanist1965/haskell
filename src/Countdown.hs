{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use splitAt" #-}
module Countdown
  ( main
  ) where


import System.IO (hFlush, stdout)
import TimedAction(timedAction)
import System.CPUTime
import GHC.Conc (readTVar)

data Op = Add | Sub | Mul | Div
data Expr = Val Int | App Op Expr Expr



apply :: Op -> Int -> Int -> Int

apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0 

eval :: Expr -> [Int]
eval (Val n) = [n| n > 0]
eval (App op l r) = [apply op x y | x <- eval l, y <- eval r, valid op x y]


getUniqueItems :: Int -> [Int] -> [[Int]]
getUniqueItems 0 _ = [[]]
getUniqueItems n xs = [x:ys | x <- xs, ys <- getUniqueItems (n-1) (filter (/= x) xs)]


choices :: [Int] -> [[Int]]
choices xs = [ y | i <- [0..length xs],  y <- getUniqueItems i xs]

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r 

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n] 

split :: [a] -> [([a], [a])]
split xs = [(take i xs, drop i xs) | i <- [1..(length xs - 1)]]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]] 

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == [n]]




-- App Sub (Val 5) (Val 10)
main :: IO ()
main = do
    putStrLn $ "Choices = " ++ show (choices [1,3,4,7, 25,75])
