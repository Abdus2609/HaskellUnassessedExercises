import Data.Char
import Data.List

type Vertex = (Float, Float)

addDigit :: Int -> Int -> Int
addDigit listnum num 
    = listnum * 10 + num

convert :: Double -> Double
convert celcius 
    = 32 + (celcius * 9/5) 

distance :: Vertex -> Vertex -> Float
distance (x1, y1) (x2, y2) 
    = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea v1 v2 v3 
    = sqrt (s * (s - a) * (s - b) * (s - c))
    where
        s = (a + b + c) / 2
        a = distance v1 v2
        b = distance v2 v3
        c = distance v1 v3

isPrime :: Int -> Bool
isPrime x
    = x > 1 && factors x == [1,x]

factors :: Int -> [Int]
factors x 
    = [n | n <- [1..x], x `mod` n == 0]

fact :: Int -> Int
fact x
    | x == 0    = 1
    | otherwise = x * fact (x - 1)

perm :: Int -> Int -> Int
perm n r 
    | r == 0    = 1
    | otherwise = perm n (r - 1) * (n - r + 1)

choose :: Int -> Int -> Int 
choose n r 
    | n == r    = 1
    | otherwise = choose (n - 1) r * (n `div` (n - r))

remainder :: Int -> Int -> Int
remainder x q 
    | q > x     = x
    | otherwise = remainder (x - q) q

quotient :: Int -> Int -> Int
quotient x d 
    | x < d     = 0 
    | otherwise = 1 + quotient (x - d) d

binary :: Int -> Int 
binary n 
    | n < 2     = n
    | otherwise = addDigit (binary (quotient n 2)) (remainder n 2)

add :: Int -> Int -> Int
add x y 
    | y == 0    = x
    | otherwise = add (succ x) (pred y)

larger :: Int -> Int -> Int
larger x y 
    | x == 0    = y
    | y == 0    = x
    | otherwise = 1 + larger (pred x) (pred y)

chop :: Int -> (Int, Int)
chop x 
    | x < 10    = (0, x)
    | otherwise = (1 + q, r)
    where
        (q, r) = chop (x - 10)

concatenate :: Int -> Int -> Int
concatenate x y
    | y == 0    = x
    | otherwise = addDigit (concatenate x q) r
    where
        (q, r) = chop y

fib :: Int -> Int
fib x 
    | x == 0    = 0
    | x == 1    = 1
    | otherwise = fib(x - 1) + fib(x - 2)

fib2 :: Int -> Int
fib2 x
    = fib2' 0 1 0
    where
        fib2' f f' k
            | k == x    = f
            | otherwise = fib2' f' (f + f') (k + 1)

goldenRatio :: Float -> Float
goldenRatio e 
    = gr 1 2 1
    where
        gr f f' r
            | abs ((r - r') / r) < e = r'
            | otherwise              = gr f' (f + f') r'
            where
                r' = fromIntegral f' / fromIntegral f

precedes :: String -> String -> Bool
precedes [] c
    = True
precedes (c : cs) (c' : cs')
    | c < c'  = True
    | c == c' = precedes cs cs'
precedes c c' 
    = False

pos :: Int -> [Int] -> Int
pos x (c : cs)
    | x == c    = 0
    | otherwise = 1 + pos x cs

twoSame :: [Int] -> Bool
twoSame [] 
    = False
twoSame (x : xs)
    = x `elem` xs || twoSame xs

rev :: [a] -> [a]
rev [] 
    = []
rev (x : xs) 
    = rev xs ++ [x] 

rev2 :: [a] -> [a]
rev2 xs
    = rev2' xs []
    where
        rev2' [] a       = a
        rev2' (x : xs) a = rev2' xs (x : a)

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of 
                                            [] -> "empty."
                                            [x] -> "a singleton list"
                                            xs -> "a longer list" 

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x : replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs

repeat' :: a -> [a]  
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x : xs) (y : ys) = (x , y) : zip' xs ys 

elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs  

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] cs
    = True
isPrefix pre str
    | pre == take n str = True
    | otherwise = False
    where
        n = length pre

removePrefix :: String -> String -> String
removePrefix pre str 
    = drop n str
    where
        n = length pre

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes str 
    = str : suffixes str'
        where
            str' = tail str

isSubstring :: String -> String -> Bool
isSubstring xs []
  = False
isSubstring substr str
    | substr `isPrefix` str || substr `elem` suffixes str = True
    | otherwise = isSubstring substr (tail str)

transpose' :: [[a]] -> [[a]]
transpose' ([] : rs)
    = []
transpose' a
    = heads a : transpose' (tails a)
    where
        heads [] = []
        heads (r : rs) = head r : heads rs
        tails [] = []
        tails (r : rs) = tail r : tails rs

removeWhiteSpace :: String -> String
removeWhiteSpace ""
    = ""
removeWhiteSpace str@(c : cs)
    = dropWhile isSpace str

nextWord :: String -> (String, String)
--Pre: first character is non-white space
nextWord ""
    = ("", "")
nextWord (c : cs)
    | isSpace c = ("", cs)
    | otherwise = (c : w, s)
    where
        (w, s) = nextWord cs

--splitUp :: String -> [String]
--splitUp "" = []
--splitUp str@(c : cs)
--    = map fst (nextWord str')
--    where
--        str' = removeWhiteSpace str

splitUp :: String -> [String]
splitUp "" = []
splitUp s
    = w : splitUp ws
    where
        (w, ws) = nextWord (removeWhiteSpace s)

primeFactors :: Int -> [Int]
--primeFactors n
--    = [x | x <- [2..n], isPrime x, n `mod` x == 0]
primeFactors n
    = factors 2 n
    where
        factors p 1
            = []
        factors p m
            | m `mod` p == 0 = p : factors p (m `div` p)
            | otherwise = factors (p + 1) m

hcf :: Int -> Int -> Int
hcf x y 
     = product [n | n <- [1..max x y], n `elem` fcsx, n `elem` fcsy]
        where
            fcsx = primeFactors x
            fcsy = primeFactors y

hcf' :: Int -> Int -> Int 
hcf' x y
    = product (fcsx \\ (fcsx \\ fcsy))
    where
        fcsx = primeFactors x
        fcsy = primeFactors y 

lcm' :: Int -> Int -> Int
lcm' x y
    = hcf x y * product(fcsx \\ fcsy) * product(fcsy \\ fcsx)
    where
        fcsx = primeFactors x
        fcsy = primeFactors y

lcm'' :: Int -> Int -> Int
lcm'' x y 
    = product (fcsn \\ fcsm) * min x y
    where
        fcsn = primeFactors(max x y)
        fcsm = primeFactors(min x y)

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove x pairings 
    = [(k, v) | (k, v) <- pairings, x /= k]

remove2 :: Eq a => a -> [(a, b)] -> [(a, b)]
remove2 x pairings 
    = filter (\(k, v) -> k /= x) pairings

remove3 :: Eq a => a -> [(a, b)] -> [(a, b)]
remove3 x pairings
    = filter ((/=x) . fst) pairings

quickSort :: [Int] -> [Int]
quickSort [] 
    = []
quickSort (x : xs)
    = smallerThan ++ [x] ++ biggerThan
    where
        smallerThan = quickSort (filter (<= x) xs)
        biggerThan = quickSort (filter (> x) xs)

allSplits :: [a] -> [([a], [a])]
allSplits list
    = [splitAt n list | n <- [1..length list - 1]]

prefixes :: [t] -> [[t]]
prefixes []
    = []
prefixes str
    = [take n str | n <- [1..length str]]

prefixes2 :: [t] -> [[t]]
prefixes2 []
    = []
prefixes2 (c : cs)
    = [c] : [c : ps | ps <- prefixes2 cs]

substrings :: String -> [String]
substrings []
    = []
substrings str
    = [i | t <- tails str, i <- tail (inits t)]

perms :: Eq a => [a] -> [[a]]
perms []
    = [[]]
perms str
    = [str' : ps | str' <- str, ps <- perms (str \\ [str'])]

routes :: Int -> Int -> [(Int, Int)] -> [[Int]]
routes start end listroutes
    | start == end = [[start]]
    | otherwise = [start : r | start'' <- [end' | (start', end') <- listroutes, start' == start], r <- routes start'' end listroutes]

routes2 :: Int -> Int -> [(Int, Int)] -> [[Int]]
routes2 start end listroutes
    = routes2' start []
    where
        routes2' start seen 
            | start `elem` seen = []
            | start == end = [[start]]
            | otherwise = [start : r | start'' <- [end' | (start', end') <- listroutes, start' == start], r <- routes start'' end listroutes]

depunctuate :: String -> String
depunctuate str
    = filter (\x -> not (x `elem` ".,:")) str

makeString :: [Int] -> String
makeString str
    = map chr str

enpower :: [Int] -> Int
enpower list
    = foldr1 (flip(^)) list

revAll :: [[a]] -> [a]
revAll list
    = concatMap reverse list

rev' :: [a] -> [a] 
rev' list
    = foldl (flip(:)) [] list 

dezip :: [(a, b)] -> ([a], [b])
dezip list
    = foldr f ([], []) list
    where
        f (x, y) (xs, ys) = (x : xs, y : ys)

allSame :: [Int] -> Bool
allSame list
    = and (zipWith (==) list (tail list))

e :: Double
e = sum (map (1/) factorials)
    where
        factorials = scanl1 (*) [1..30]

squash :: (a -> a -> b) -> [a] -> [b]
squash f list 
    = zipWith f list (tail list)

converge :: (a -> a -> Bool) -> [a] -> a
--Pre: [a] contains one element
converge f [x] 
    = x
converge f (x : y : ys)
    | f x y     = y
    | otherwise = converge f (y : ys)

limit :: (a -> a -> Bool) -> [a] -> [a]
limit f list@(x : y : ys)
    | f x y = [x, y]
    | otherwise = x : limit f (y : ys) 
limit f xs
    = xs

repeatUntil :: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil condition f x
    = head (filter condition (iterate f x))

repeatUntil' :: (a -> Bool) -> (a -> a) -> a -> a 
repeatUntil' condition f 
    = head . filter condition . iterate f

all' :: (a -> Bool) -> [a] -> Bool 
all' condition list 
    = and (map condition list)

any' :: (a -> Bool) -> [a] -> Bool
any' condition list 
    = or (map condition list)

isElem :: Eq a => a -> [a] -> Bool
isElem x list
    = any' (==x) list

isElem' :: Eq a => a -> [a] -> Bool
isElem' 
    = any' . (==)

pipeline :: [a -> a] -> [a] -> [a]
pipeline 
    = map . foldr (.) id

data Shape = Triangle Float Float Float | Square Float | Circle Float | Polygon [Vertex]

area :: Shape -> Float
area (Triangle a b c)
    = triangleArea' a b c
area (Square s)
    = s ^ 2
area (Circle r)
    = pi * (r ^ 2)
area (Polygon vs)
    = polygonArea vs 

triangleArea' :: Float -> Float -> Float -> Float 
triangleArea' a b c    
    = sqrt(s * (s - a) * (s - b) * (s - c))
    where
        s = 1/2 * (a + b + c)

polygonArea :: [(Float, Float)] -> Float
polygonArea (v1 : v2 : v3 : vs) 
    = triangleArea' a b c + polygonArea (v1 : v3 : vs)
    where
        a = dist v1 v2
        b = dist v2 v3
        c = dist v3 v1
        dist (x1, y1) (x2, y2) = sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
polygonArea vs
    = 0

type Date = (Int, Int, Int)

age :: Date -> Date -> Int 
age (d1, m1, y1) (d2, m2, y2)
    | (m1, d1) <= (m2, d2) = y2 - y1 
    | otherwise = y2 - y1 - 1

data Tree = Leaf | Node Tree Tree 
    deriving (Eq, Show)

makeTrees :: Int -> [Tree]
makeTrees n
    | n == 0 = [Leaf]
    | otherwise = [Node l r | (t, t') <- map makeTrees' [0..n-1], l <- t, r <- t']
    where
        makeTrees' k = (makeTrees k, makeTrees (n - k - 1))

makeTrees2 :: Int -> [Tree]
makeTrees2 n
    = makeTrees2' n [[Leaf]]
    where
    makeTrees2' k ts
        | k == 0 = head ts
        | otherwise = makeTrees2' (k - 1) (ts' : ts)
        where
            ts' = [Node l r | (t, t') <- zip ts (reverse ts), l <- t, r <- t']

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
            deriving (Show)

build :: [a] -> Tree' a
build []
    = error "empty tree"
build [x]
    = Leaf' x
build xs
    = Node' (build left) (build right)
    where
        (left, right) = splitAt (length xs `div` 2) xs

ends :: Tree' a -> [a]
ends (Leaf' x)
    = [x]
ends (Node' left right)
    = ends left ++ ends right

swap :: Tree' a -> Tree' a 
swap (Leaf' x)
    = Leaf' x
swap (Node' left right)
    = Node' (swap right) (swap left)

data Tree'' a b = Empty | Leaf'' b | Node'' a (Tree'' a b) (Tree'' a b)

mapT :: (a -> a) -> (b -> b) -> Tree'' b a -> Tree'' b a
mapT f g Empty 
    = Empty 
mapT f g (Leaf'' x)
    = Leaf'' (f x)
mapT f g (Node'' x left right)
    = Node'' (g x) (mapT f g left) (mapT f g right)

foldT :: (a -> b) -> (c -> b -> b -> b) -> b -> Tree''  c a -> b 
foldT f g x Empty
    = x 
foldT f g x (Leaf'' y)
    = f y
foldT f g x (Node'' y left right)
    = g y (foldT f g x left) (foldT f g x right)

countLeaves :: Tree'' a b -> Int
countLeaves 
    = foldT (const 1) (const (+)) 0

sum' :: Tree'' Int Int -> Int   
sum' 
    = foldT id (\n x y -> n + x + y) 0

flattenLR :: Tree'' a a -> [a]
flattenLR 
    = foldT (: []) (\n x y -> x ++ [n] ++ y) []

flattenRL :: Tree'' a a -> [a]
flattenRL 
    = foldT (: []) (\n x y -> y ++ [n] ++ x) []

eval :: Tree'' (Int -> Int -> Int) Int -> Int
eval 
    = foldT id id 0

data Colour = Red | Green | Blue 
    deriving (Show, Bounded, Enum)

data AmPm = AM | PM 

data Time = TwentyFour Int Int | TwelveHour Int Int AmPm

instance Eq Time where 
    (==) = equalTime

instance Show Time where 
    show (TwelveHour 12 00 AM) = "Midnight"
    show (TwelveHour 12 00 PM) = "Midday"
    show (TwelveHour h m AM) = show' h ++ ":" ++ show' m ++ "AM"
    show (TwelveHour h m PM) = show' h ++ ":" ++ show' m ++ "PM"
    show (TwentyFour h m) = show' h ++ show' m ++ "HRS"
    
show' :: Int -> String
show' n
    | n < 10 = "0" ++ show n
    | otherwise = show n

to24 :: Time -> Time 
to24 (TwentyFour h m)
    = TwentyFour h m 
to24 (TwelveHour h m AM)
    = TwentyFour h m 
to24 (TwelveHour h m PM)
    = TwentyFour (h + 12) m

equalTime :: Time -> Time -> Bool 
equalTime time1 time2 
    = isSame (to24 time1) (to24 time2)
    where
        isSame (TwentyFour h1 m1) (TwentyFour h2 m2)
            = h1 == h2 && m1 == m2 