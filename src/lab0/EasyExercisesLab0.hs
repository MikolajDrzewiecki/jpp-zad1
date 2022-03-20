module EasyExercisesLab0 (
  countdown,
  collatz,
  myLast,
) where
  

-- Napisz funkcję countdown, która dla danej liczby naruralnej będzie "odliczać" od tej liczby do 0,
countdown :: Int -> [Int]
countdown 0 = [0]
countdown n = let xs = countdown (n - 1) in (n:xs)


-- Napisz funkcję collatz, która dla danej liczby da jej sekwencję Collatza,
collatz :: Int -> [Int]
collatz n
  | n == 1 = [1]
  | even n = let ns = collatz (n `div` 2) in n:ns
  | odd n = let ns = collatz (3 * n + 1) in n:ns


-- Standardowa funkcja last daje ostatni element swojego argumentu. Czy potrafisz napisać własną implementację tej funkcji?
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

