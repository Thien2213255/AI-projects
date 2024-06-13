double x = x + x 
quadruple x = double(double x)
factorial n = product [1..n]         --cách định nghĩa hàm 1 : hàm - tham số = định nghĩa cho hàm
average ns = sum ns `div` length ns
add :: (Int, Int) -> Int --cách định nghĩa hàm 2 : hàm :: (kiểu dữ liệu) -> (kiểu dữ liệu)
add(x,y) = x+y           --ở bên dưới thì : hàm(tham số) = định nghĩa cho hàm
zeroto :: Int -> [Int]
zeroto(x) = [0..x]
abs1 :: Int -> Int 
abs1 n = if n >= 0 then n else -n
signum1 :: Int -> Int 
signum1 n = if n < 0 then -1 else 
            if n > 0 then 1 else 0
signum2 :: Int -> Int
signum2 n   | n < 0 = -1
            | n > 0 = 1
            | otherwise = 0
concat1 :: [[a]] -> [a]
concat1 xss = [x | xs <- xss, x <- xs]
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
prime :: Int -> Bool 
prime n = factors n == [1,n]
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]
--zip ['a', 'b', 'c'] [1,2,3] => [('a',1),('b',2),('c',3)]
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
sorted :: Ord a => [a] -> Bool 
sorted xs = and [x <= y | (x,y) <- pairs xs]
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x==x']
count1 :: Char -> String -> Int 
count1 x xs = length [x' | x' <- xs, x == x']
fac1 0 = 1
fac1 n = n*fac1(n-1)

product1 :: Num a => [a] -> a 
product1 [] = 1
product1 (n:ns) = n * product1(ns)

length1 :: Num a => [a] -> a 
length1 [] = 0
length1(_:xs) = 1 + length1(xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys 
merge xs [] = xs 
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) (ys)