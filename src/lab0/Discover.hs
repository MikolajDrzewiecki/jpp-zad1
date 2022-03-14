module Discover (
  fact,
  len,
  myZip,
  rd,
  splitBy,
  splitBy',
  (+++)
) where

  fact :: Int -> Int                          -- n!
  len :: [a] -> Int                           -- Length of a list
  myZip :: [a] -> [b] -> [(a, b)]             -- my implementation of zip
  rd :: [a] -> Either Bool a                  -- third element of a list
  splitBy :: Int -> [Int] -> ([Int],[Int])    -- splits a list by a number
  splitBy' :: Int -> [Int] -> ([Int],[Int])   -- splits a list by a number
  
  infixl 6 +++                                -- gives priority 6 (same as +) to +++ operator
  (+++) :: Int -> Int -> Int                  -- new +++ operator
  
  
  fact 0 = 1
  fact n = n * fact (n - 1)

  len [] = 0
  len (_:xs) = 1 + len xs

  myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
  myZip _ _ = []

  rd (_:_:z:_) = Right z
  rd _ = Left False
  
  splitBy n (x:xs)
    | x <= n = let (ys,zs) = splitBy n xs in (x:ys,zs)
    | x > n = let (ys,zs) = splitBy n xs in (ys,x:zs)
  splitBy _ [] = ([],[])
   
  splitBy' n (x:xs) 
    | x <= n = (x:ys,zs)
    | otherwise = (ys,x:zs)
    where (ys,zs) = splitBy' n xs
  splitBy' _ [] = ([],[])
  
  x +++ y = (x + y) * (x + y + 1) `div` 2