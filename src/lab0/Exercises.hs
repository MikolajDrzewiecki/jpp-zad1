module Exercises (
  head',
  tail',
  (++++),
  (+++),
  take',
  drop',
  filter',
  map',
  concat',
  inits,
  partitions,
  permutations,
  nub,
) where


-- Napisz własne odpowiedniki standardowych funkcji: head
head' :: [a] -> a
head' (x:_) = x


-- Napisz własne odpowiedniki standardowych funkcji: tail
tail' :: [a] -> [a]
tail' (_:xs) = xs


-- Napisz własne odpowiedniki standardowych funkcji: ++
(++++) :: [a] -> [a] -> [a]
(++++) xs ys = foldr (:) ys xs


-- Napisz własne odpowiedniki standardowych funkcji: ++ (drugi sposób)
(+++) :: [a] -> [a] -> [a]
(+++) [] ys = ys
(+++) (x:xs) ys = let zs = (++++) xs ys in x:zs


-- Napisz własne odpowiedniki standardowych funkcji: take
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = let zs = take' (n-1) xs in x:zs


-- Napisz własne odpowiedniki standardowych funkcji: drop
drop' :: Int -> [a] -> [a]
drop' n xs
  | n <= 0 = xs
  | otherwise = drop'' n xs
  where
    drop'' :: Int -> [a] -> [a]
    drop'' 0 ys = ys
    drop'' _ [] = []
    drop'' m (_:ys) = drop'' (m-1) ys


-- Napisz własne odpowiedniki standardowych funkcji: filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]


-- Napisz własne odpowiedniki standardowych funkcji: map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = let ys = map' f xs in (f x) : ys


-- Napisz własne odpowiedniki standardowych funkcji: concat
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = let ys = concat' xs in x ++ ys

-- Napisz funkcję inits, ktora dla danej listy da listę wszystkich jej odcinków początkowych
inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = let ys = inits xs in [x] : map (x:) ys


-- Napisz funkcje partitions, ktora dla danej listy xs da liste wszystkich par (ys,zs) takich, że xs == ys ++ zs
-- Krótszy sposób
partitions :: [a] -> [([a], [a])]
partitions [] = [([], [])]
partitions (x:xs) = ([], x:xs) : [(x:ys, zs) | (ys, zs) <- partitions xs]


-- Napisz funkcje permutations, która dla danej listy da listę wszystkich jej permutacji
-- (dla uniknięcia niejasności możemy założyć, ze wszystkie elementy listy wejściowej są różne)
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [[z] ++ ws  | (ys, z:zs) <- partitions xs, ws <- permutations (ys ++ zs)]


-- Napisz funkcje nub, ktora usunie z listy wszystkie duplikaty
nub :: [a] -> [a]
nub [] = []

