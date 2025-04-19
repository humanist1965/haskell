import Debug.Trace

fac :: Integer -> Integer
fac = fac' 1                -- Eta-reduction

fac' :: Integer -> Integer -> Integer
fac' res 0 = res
fac' res n = fac' (res * n) (n - 1)


main2 = let x = 3
            y = 5
                in x + y  

main3 = (let
 _ = print "fff";
 x = 3;
 y = 15 in
 (let x = 4 in x + y)) 


main :: IO ()
main = do
  print "fff"
  let x = 3
      y = 10
  result <- return (x + y)  -- Compute pure expression
  print result 

main4 :: IO ()
main4 = do
  print "fff"
  result <- 
    let x = 3 in do
        print "**1"
        let y = 10 in do
            print "**2"
            return (x + y)  -- Compute pure expression
  print result  


main5 :: IO ()
main5 = do
  let x = trace "x = 3" 3
      y = trace "y = 10" 20
      result = trace ("result = " ++ show (x + y)) (x + y)
  print result