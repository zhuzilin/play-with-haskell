import Control.Monad (replicateM)

-- 1
myLast :: [a] -> a
myLast [] = error "No end for empty list!"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast = head . reverse . init

-- 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list!"
elementAt (x:xs) n = if n == 1 then x else elementAt xs (n-1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse a = tmpReverse a []

tmpReverse :: [a] -> [a] -> [a]
tmpReverse [] ys = ys
tmpReverse (x:xs) ys = tmpReverse xs (x:ys)

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8
compress :: Eq a => [a] -> [a]
compress xs = compress' xs []

compress' :: Eq a => [a] -> [a] -> [a]
compress' [] xs = reverse xs
compress' [x] xs = x:xs
compress' (x1:x2:xs) ys = if x1 == x2 
                       then compress' (x2:xs) ys
                       else compress' (x2:xs) (x1:ys)

-- 9
pack :: Eq a => [a] -> [[a]]
pack xs = pack' xs []

pack' :: Eq a => [a] -> [[a]] -> [[a]]
pack' [] ys = reverse ys
pack' (x:xs) [] = pack' xs [[x]]
pack' (x:xs) (y:ys) = if x == head y then pack' xs ((x:y):ys)
                                     else pack' xs (([x]):(y:ys))

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = encode' xs []

encode' :: Eq a => [a] -> [(Int, a)] -> [(Int, a)]
encode' [] ys = reverse ys
encode' (x:xs) [] = encode' xs [(1, x)]
encode' (x:xs) ((cnt, y):ys) = if x == y then encode' xs ((cnt+1, y):ys)
                                     else encode' xs ((1, x):(cnt, y):ys)

-- 11
-- assit func: map
data ListItem a = Single a | Multiple Int a
    deriving (Show)
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
        encodeHelper (1, x) = Single x
        encodeHelper (n, x) = Multiple n x

-- 12
-- assist func: concatMap, replicate
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where decodeHelper (Single x) = [x]
          decodeHelper (Multiple n x) = replicate n x

-- 13
-- 注意这种写法，用一个辅助函数参数来进行尾递归，以及下面用一个函数作为辅助的类型
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' 1 x xs

encodeDirect' n y [] = [encodeElement n y]
encodeDirect' n y (x:xs) | y == x = encodeDirect' (n+1) y xs
                         | otherwise = (encodeElement n y) : (encodeDirect' 1 x xs)
encodeElement 1 x = Single x
encodeElement n x = Multiple n x
            
-- 14
-- assist func: concatMap, replicate
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- 15
-- 和14一样
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- 16
-- 和13的辅助参数类似，注意可以通过添加一个参数的方式存储静态变量
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' n 1 xs []
    where 
        dropEvery' _ _ [] ys = reverse ys
        dropEvery' n m (x:xs) ys | n == m = dropEvery' n 1 xs ys
                                 | otherwise = dropEvery' n (m+1) xs (x:ys)

-- 17
-- one line solution:
-- split xs n = (take n xs, drop n xs)
split :: [a] -> Int -> ([a], [a])
split xs n = split' n xs []
    where split' 0 xs ys = (reverse ys, xs)
          split' n (x:xs) ys = split' (n-1) xs (x:ys)

-- 18
-- assist func: take, drop
slice :: [a] -> Int -> Int -> [a]
slice xs l r = drop (l-1) (take r xs)

-- 19
rotate :: [a] -> Int -> [a]
rotate xs n | n > 0 = (drop n xs) ++ (take n xs)
            | otherwise = rotate xs (n + (length xs))

-- 20
-- assist func: (!!) 用于indexing
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys 1 = x:ys
insertAt x (y:ys) n = y:insertAt x ys (n-1)

-- 22
-- range x y = [x..y]
range :: Int -> Int -> [Int]
range a b | a > b = []
          | otherwise = a : range (a+1) b

-- 23
-- somthing with random









-- 31
isPrime :: Int -> Bool
isPrime n = length [k | k <- [2..n], n `mod` k == 0] == 1

-- 32
myGCD :: Int -> Int -> Int
myGCD a 0 = a
myGCD a b | a < 0 = myGCD (-a) b
          | b < 0 = myGCD a (-b)
          | otherwise = myGCD b (a `mod` b)

-- 33
coprime a b = gcd a b == 1

-- 34
totient n = length [k | k <- [1..n], coprime n k ]

-- 35
primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2
    where
        primeFactors' n k
            | k*k > n = [n]
            | n `mod` k == 0 = k : primeFactors' (n `div` k) k
            | otherwise = primeFactors' n (k+1)

-- 36
primeFactorsMult n = map swap $ encode $ primeFactors n
    where swap (x, y) = (y, x)

-- 37
phi n = foldr f 1 $ primeFactorsMult n
    where
        f (p, m) x = x * (p-1) * (p^(m-1))

-- 38
-- 比较37和34

-- 39
-- 这里用筛法，充分利用了lazy evaluation，很酷
-- assist func: takeWhile, dropWhile
primesR l r = takeWhile (<= r) $ dropWhile (< l) $ sieve [2..]
    where
        sieve (n:ns) = n : sieve [m | m <- ns, m `mod` n /= 0]

-- 40
goldbach n = head [(a, n-a) | a <- [1..n], isPrime a && isPrime (n-a) ]

-- 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList l r = map goldbach (let even = (((l + 1) `div` 2) * 2)
                                 in [even,(even+2)..r])

-- 46
-- assist func: mapM_ (注意mapM, mapM_, map之间的差别)
-- map :: (a -> b) -> [a] -> [b]
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b) |
                            a <- [True, False], b <- [True, False]]

-- 47
-- 没有去写46需要的表，所以也就不写47了

-- 48
-- assist func replicateM, unword
-- replicateM :: (Applicative m) => Int -> m a -> m [a]
-- replicateM cnt0 f =
--     loop cnt0
--   where
--     loop cnt
--         | cnt <= 0  = pure []
--         | otherwise = liftA2 (:) f (loop (cnt - 1))
-- liftA2就是<*>
-- unword: creates a string from an array of strings, 
--         it inserts space characters between original strings
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
    where args n = replicateM n [True, False]
          toStr = unwords . map (\x -> show x ++ space x)
          space True = "  "
          space False = " "

-- 49
gray :: Int -> [String]
gray 0 = [""]
gray n = ['0':x | x <- prev] ++ ['1':x | x <- prev]
        where prev = gray (n-1)

-- 50



-- 54A
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
-- Haskell的type system自动保证就是binary tree

-- 55
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = if n `mod` 2 == 1
             then [Branch 'x' l r | l <- cbalTree (n `div` 2),
                                    r <- cbalTree (n `div` 2)]
             else concat 
                  [ [Branch 'x' l r, Branch 'x' r l] | l <- cbalTree (n `div` 2),
                                                       r <- cbalTree ((n-1) `div` 2)]
-- 56
symmetric :: Tree a -> Bool
symmetric t = mirror t t
    where mirror Empty Empty = True
          mirror Empty _     = False
          mirror _     Empty = False
          mirror (Branch _ a b) (Branch _ c d) = mirror a d && mirror b c

-- 57
construct :: Ord a => [a] -> Tree a
construct xs = foldl insert Empty xs
    where insert Empty x = (Branch x Empty Empty)
          insert (Branch y a b) x 
                              | x < y     = (Branch y (insert a x) b)
                              | otherwise = (Branch y a (insert b x))

-- 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees n | n `mod` 2 == 0 = []
               | otherwise      = [Branch 'x' t (reverseTree t) | t <- cbalTree (n `div` 2)]
               where
                    reverseTree Empty = Empty
                    reverseTree (Branch x a b) = Branch x (reverseTree b) (reverseTree a)

-- 59



-- 60



tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)
-- 61
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ a b) = countLeaves a + countLeaves b

-- 61A
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch x a b) = (leaves a) ++ (leaves b)

-- 62
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x a b) = x : (internals a) ++ (internals b)

-- 62B
atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch _ a b) n = (atLevel a (n-1)) ++ (atLevel b (n-1))

-- 63








-- 93

