{-
READ ME: Change the name of this file to YOURSTUDENTNUMBER.hs. For example, if your
student number were 123456789, then you would rename the file 123456789.hs.

REPLACE the function definitions for each of the questions. 
The names of the functions correspond to the names given in the document set07016_cwk_16_17.pdf. 

DO NOT CHANGE THE TYPE SIGNATURES, OR THE ORDER OF THE ARGUMENTS!

You may add as many extra or helper functions as you wish, but do not make any "import" statements.
-}

uniqueList :: (Eq a) => [a] -> [a]
uniqueList [] = []
uniqueList (x:xs)
    | elem x xs = uniqueList xs
    | otherwise = x : uniqueList xs


isSubset :: (Eq a) => [a] -> [a] -> Bool
isSubset [] _ = True
isSubset (x:xs) ys 
   | elem x ys = isSubset xs ys
   | otherwise = False
   
removeDuplicates :: (Eq a) => [a] -> [a] -> Maybe [a]
removeDuplicates [] ys = Just ys
removeDuplicates (x:xs) ys = removeDuplicates xs (filter (/=x) ys)
   
complement :: (Eq a) => [a] -> [a] -> Maybe [a]
complement [] ys = Just ys
complement xs ys
   | isSubset xs ys = removeDuplicates xs ys
   | otherwise = Nothing

addAtEnd :: a -> [a] -> [a]
addAtEnd x [] = [x]
addAtEnd x (y:ys) = y : addAtEnd x ys 

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = append xs (addAtEnd x ys) 

count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) | x==y = 1+(count x ys)
               | otherwise = count x ys

remove _ [] = []
remove x (y:ys) 
		| x == y = remove x ys
        | otherwise = y : remove x ys

mUnion :: (Eq a) => [a] -> [a] -> [a]
mUnion [] ys = ys
mUnion xs [] = xs
mUnion (x:xs) (y:ys)
	| count x (x:xs) > count x (y:ys) =  (replicate ((count x (x:xs))) x) ++ mUnion (remove x (x:xs)) (remove x (y:ys)) 
	| count x (y:ys) > count x (x:xs) = (replicate ((count x (y:ys))) x) ++ mUnion (remove x (x:xs)) (remove x (y:ys)) 
	| otherwise = (replicate ((count x (x:xs))) x) ++ mUnion (remove x (x:xs)) (remove x (y:ys))
	
{-
Your functions should have the following behaviour:
complement [1,2,3] [1..5] = Just [4,5]
complement [1,2,3] [2..5] = Nothing
mUnion [1,1,1,2] [1,2,2,3,4,4] = [1,1,1,2,2,3,4,4]
mUnion [1,4,1] [4,4,1] = [1,1,4,4]

THE ORDER OF ELEMENTS IN THE RESULTS OF mUnion IS NOT IMPORTANT.
-}



-- QUESTION 2: Functions and relations

getFirstElements :: [(a,a)] -> [a]
getFirstElements lst = [fst x | x <- lst]

getSecondElements :: [(a,a)] -> [a]
getSecondElements lst = [snd x | x <- lst] 

mapToEverything :: [a] -> [(a,a)]
mapToEverything [] = []
mapToEverything (y:ys) = (y,y)  : mapToEverything ys

listOfEverything :: [(a,a)] -> [(a,a)]
listOfEverything xs = mapToEverything (getSecondElements xs) ++ mapToEverything (getFirstElements xs)


reflClosure :: (Eq a) => [(a,a)] -> [(a,a)]
reflClosure xs = uniqueList(xs ++ uniqueList(listOfEverything xs))



-- TEST SET FOR Q2
{-
Your functions should have the following behaviour:
reflClosure [(1,2),(3,2)] = [(1,2),(3,2),(1,1),(2,2),(3,3)]
reflClosure [(1,1),(3,5)] = [(1,1),(3,5),(3,3),(5,5)]


DO NOT WORRY ABOUT THE ORDER IN WHICH PAIRS APPEAR IN YOUR LIST
-}

-- QUESTION 3: Combinatorics

choose2 :: [Int] -> [(Int,Int)]
choose2 xs = [(x,y) | x <- xs, y <- xs, x < y]

-- TEST SET FOR Q3
{-
Your functions should have the following behaviour:
choose2 [1,2,3] = [(1,2),(1,3),(2,3)]
choose2 [2,6,9,12] = [(2,6),(2,9),(2,12),(6,9),(6,12),(9,12)]
NOTE THAT THE SMALLER ELEMENT IN EACH PAIR APPEARS FIRST. THE ORDERING OF THE PAIRS IN THE LIST DOES NOT MATTER.
-}


-- QUESTION 4: Primes

factors :: Int -> [Int]
factors n = factorsGreaterThanOrEqual 2
	where 
		factorsGreaterThanOrEqual x
			| (x == n) = [n]
			| (n `mod` x == 0) = x : otherFactors
			| otherwise = otherFactors
			where otherFactors = factorsGreaterThanOrEqual (x + 1)

primeFactorisation :: Int -> [Int]
primeFactorisation 1 = []
primeFactorisation n =
	case factors of 
	[] -> [n] 
	_ -> factors ++ primeFactorisation (n `div` (head factors)) 
	where factors = take 1 $ filter (\x -> (n `mod` x)==0) [2 .. n-1]
	
divides :: Int -> Int -> Bool
divides x n = rem x n == 0

isComposite :: Int -> Bool
isComposite n = foldl (||) False (map (divides n) [2..(n-1)])

isPrime :: Int -> Bool
isPrime n
        | n <=0 = error "Makes no sense"
        | otherwise = not (isComposite n)
		
nextPrime :: Int -> Int
nextPrime n  
	| isPrime (n) = n
	| otherwise = nextPrime (n + 1)


-- TEST SET FOR Q4
{-
Your functions should have the following behaviour:
factors 75 = [3,5,15,25,75]
factors 64 = [2,4,8,16,32,64]
primeFactorisation 75 = [3,5,5]
primeFactorisation 64 = [2,2,2,2,2,2]
nextPrime 25 = 29
nextPrime 400 = 401
-}


-- QUESTION 5: RSA

eTotient :: Int -> Int
eTotient n = length [x | x <- [1..n], coprime x n]
	where coprime n x = gcd n x == 1

encode :: Int -> Int -> Int -> Int -> Maybe Int
encode p q m e
	| isPrime p && isPrime q && gcd e ((p-1)*(q-1)) == 1 = Just (mod(m ^ e)(p * q))
	| otherwise = Nothing 
	

-- TEST SET FOR Q5
{-
Your functions should have the following behaviour:
eTotient 54 = 18
eTotient 73 = 72
encode 53 73 151 95 = Just 3689
encode 99 18 108 45 = Nothing
encode 37 17 23 48 = Nothing
-}


