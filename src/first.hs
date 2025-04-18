

fac :: Integer -> Integer
fac = fac' 1                -- Eta-reduction

fac' :: Integer -> Integer -> Integer
fac' res 0 = res
fac' res n = fac' (res * n) (n - 1)

