module EasyExercisesLab0 (
  countdown,
  collatz,
  myLast,
) where


countdown :: Int -> [Int]
collatz :: Int -> [Int]
myLast :: [a] -> a


countdown 0 = [0]
countdown n = let xs = countdown (n - 1) in (n:xs)

collatz n
  | n == 1 = [1]
  | even n = let ns = collatz (n `div` 2) in n:ns
  | odd n = let ns = collatz (3 * n + 1) in n:ns

myLast [x] = x
myLast (_:xs) = myLast xs

