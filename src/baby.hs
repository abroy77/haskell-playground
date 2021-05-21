-- first line is is a declarator with the input and return type and constraints. 
doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

-- there is if else. but usually you'll use guards '|'. shown below | | | |
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

-- function can act as a variable/ const as well. is an expression essentially
conanO'Brien :: [Char]
conanO'Brien = "It's a-me, Conan O'Brien!"


-- list comprehension. thing before '|' is the output funtion
-- after '|' there's stuff with '<-' these are the inputs <var> <- <[vars]>
-- after inputs you have your input filters. 
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]


addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1 .. n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- this is pattern matching. kind of like using if/else or cases or like overloading.
-- helps curate function for different inputs
-- rather than having loads of if else conditions.
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"


-- example of pattern matching used to make a recursive function. first pattern is edge case

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

-- pattern matching is very versatile. pattern tells you you're expecting 2 2-dim tuples
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- underscore '_' is used when you don't care what that part of the pattern in. 
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x : _) = x

-- cool way to make a length funciton. essentially for each element of input list, the output
-- list has 1. so adding all the ones gives len of list. coolio!
length1 :: Integral a => [b] -> a
length1 xs = sum [1 | _ <- xs]

length' :: Integral a => [b] -> a
length' [] = 0
length' (_ : xs) = 1 + length' xs


-- recursive summation function for a list
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y


-- the '@' is used when you want to store the full pattern and the disected pattern
-- before '@' stores the whole input. after '@' stores the broken bits
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- Guards!!! essentially the '|'s are if clauses. 
-- every '|' after the first is like elseif
-- 'otherwise' is the else condition
--syntax is <condition> = <value>.... where condition is boolean
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

-- the backticks are used to define an infix function. like 5'+'3. addition is infix
-- usage requires the backticks. like 6 `myCompare` 9
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT


-- where is used to assign some values that are in the scope of the guards as well. they don't exist
-- outside this function though.
bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0


-- guards here would be nicer but we're using this as an example of where 
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

-- you can have functions (expressions) within where. how cool
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2


-- let is like where but very local. only exists in the 'in' part. 
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  


-- like guards. is switch cases essentially. weird syntax tbh

head1 :: [a] -> a  
head1 xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  


-- now we in recursion bit



-- If you don't understand recursion, read this sentence again. 

-- now we doin recursion if you didn't figure out yet. 
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum1 :: (Ord a) => [a] -> a  
maximum1 [] = error "maximum of empty list"  
maximum1 [x] = x  
maximum1 (x:xs) = max x (maximum' xs) 


replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  



take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs 

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  


-- this would give infinite repition. 
-- haskell is lazy. so if you run take 5 (replicate 3). it won't crash! wow. 
-- only computer as much as it needs to.
repeat' :: a -> [a]  
repeat' x = x:repeat' x  


zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs

{-|
   a sorted list is a list that has all the values smaller than (or equal to) 
   the head of the list in front (and those values are sorted), then comes the 
   head of the list in the middle and then come all the values that are bigger 
   than the head (they're also sorted)
-}
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 