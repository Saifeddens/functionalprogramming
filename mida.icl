module mida
import StdEnv

/* WRITE NAME AND NEPTUN CODE HERE
Saifeddin Alkurdi o9ffdc
by this YOU DECLARE this FILE is 
YOUR OWN SOLUTION for functional 
programming midterm 2024 April 17.*/

/*1. Bacteria
Let's say that a bacterium is dividing every 10 minutes, which means that
every 10 minutes it will double into two bacteria. You are given two
numbers: the starting number of bacteria and the target time interval. 
You need to return how many bacteria we will have in n minutes
(assume n is divisible by 10).

bacteria_nr 1 40 -> 16
Explanation:
We start with 1 bacterium.
In 10 minutes there are 1*2 = 2.
In 20 minutes there are 2*2 = 4.
In 30 minutes there are 4*2 = 8.
In 40 minutes there are 8*2 = 16.
*/
/*
bacteria_nr :: Int Int -> Int
bacteria_nr x y 
| (x*2) 
= []
*/
//Start = bacteria_nr 1 40 // 16
//Start = bacteria_nr 5 40 // 80
//Start = bacteria_nr 1 100 // 1024
//Start = bacteria_nr 7 70 // 896
//Start = bacteria_nr 9 150 // 294912


/*2. Ackermann function
Implement the Ackermann function, which is a classic example of 
a function that grows extremely quickly.
It is defined recursively as:

A(m, n) = 
- n + 1                  if m = 0
- A(m - 1, 1)            if m > 0 and n = 0
- A(m - 1, A(m, n - 1))  if m > 0 and n > 0
*/

ackermann :: (Int, Int) -> Int 
ackermann (m,n)
| (m == 0) = n+1
| (m > 0) && (n==0) = ackermann (m-1,1)
| (m > 0) && (n > 0)= ackermann (m-1,ackermann (m,n-1))

//Start = ackermann (0,0) // 1
//Start = ackermann (1,1) // 3
//Start = ackermann(3,3) // 61
//Start = ackermann (1,2) // 4


/*3. Largest number
Given a number, by changing the order of the digits construct 
the largest possible number. 

1234 -> 4321 is the largest number made from digits 1,2,3 and 4 
34565 -> 65543 */

tolist :: Int -> [Int]
tolist x
| x<10 = [x]
=tolist(x/10) ++ [(x rem 10)]
//Start = tolist 123

tonum :: [Int] -> Int
tonum [x:xs] = sum[a^b\\a<-[x:xs]& b<-[length [x:xs],length [x:xs]-1..]]
//| length [x:xs]==1 = x
//=x*10^(length [x:xs]-1) + (10 rem x) + tonum xs

//Start=tonum [1,2,3,4]

largestNum :: Int -> Int
largestNum x = toInt(tonum(reverse(sort(tolist x))))
//Start = largestNum 1234 // 4321
//Start = largestNum 370293843 // 987433320


/*4. Extract
Write a function that extracts a sublist from a list. The first number is the
starting index, and the second one is the length of the sublist.
*/

sublist :: Int Int [Int] -> [Int]
sublist x y z = [z!!a\\ a<-[x..(x+y)-1]]
//Start = sublist 2 3 [1..7] // [3,4,5]
//Start = sublist 0 1 [1..7] // [1]
//Start = sublist 0 0 [1..7] // []
//Start = sublist 0 7 [1..7] // [1,2,3,4,5,6,7]
//Start = sublist 10 5 [1..100] // [11,12,13,14,15]
//Start = sublist 4 3 [5, 8, 32, 7, 2, 6, 9, 12, 52, 3] // [2,6,9]
//Start = sublist 2 5 [5, 8, 32, 7, 2, 6, 9, 12, 52, 3] // [32,7,2,6,9]


/*5. Triple tuple
Write a function that takes a list of triple tuples (Int,Char,[Int]) 
and creates a single tuple with ([Int],[Char],[[Int]]).
*/
/*
ft3 :: [(Int,Char,[Int])] -> ([Int],[Char],[[Int]])
ft3 [(a,b,c)]
| length [(a,b,c)]==1 = [([a],[b],[c])] 
= (map zip2(a,b,c))
*/			
//Start = ft3 [(1,'a',[1..10]),(2,'b',[2..9]),(3,'c',[3..7])] 
// ([1,2,3],['a','b','c'],[[1,2,3,4,5,6,7,8,9,10],[2,3,4,5,6,7,8,9],[3,4,5,6,7]])
//Start = ft3 [(1,'a',[1,3,4])] // ([1],['a'],[[1,3,4]])


/*6. Good lists
You are given a list of lists of integers. First, remove all those sublists whose
maximum absolute value is an Odd number. Afterwards, transform all remaining sublists 
which have more than 3 numbers to "good" and those who don't to "bad".

Input: [[-8,5,3],[92,33,-95],[64,86]]
Step 1: Remove lists where the maximum absolute value is odd
[-8,5,3] => we keep this, because the maximum absolute value is 8. 8 is even
[92,33,-95] => we do not keep this, because maximum absolute value is -95. -95 is odd
[64,86] => we keep this, because maximum absolute value is 86. 86 is even
Step 2: Transform the lists that are kept to "good" or "bad"
[-8,5,3] => "Good", because it has more than 2 elements
[64,68] => "Bad", because it does not have more than 2 elements
*/

checkif :: [Int] -> [Int]
checkif x 
| isEven(maxList[abs(a)\\a<-x]) = x 
| isOdd(maxList[abs(a)\\a<-x]) = [-1]++x

checkiff :: [Int] -> String
checkiff x 
| length x > 2 = "good"
="bad"

//Start = checkif [1,2]

processLists :: [[Int]] -> [String]
processLists x = [ checkiff a\\a<-(map checkif x) | hd a <> -1]

//Start = processLists [[-8,5,3],[92,33,-95],[64,86]] // ["good","bad"]
//Start = processLists [[1,1,1,1],[98,-99,72],[100]] // ["bad"]
//Start = processLists [[100,100],[2,2],[-90,-90,-90]] // ["bad","bad","good"]
//Start = processLists [[-3,5,-71],[3],[5]] // []


/*7. Special sorted strings
Create a function that takes two lists of tuples of the same length, 
each containing a string and a special character. 
Compare tuples at the same position in both lists and 
keep the one with the smaller character.
 
Comparing ("Good", 'c') and ("Happy", 'z') would retain ("Good", 'c') because 'c' < 'z'. 
Then, only keep the strings with characters greater than 
the special character, resulting in a list of strings.
*/
/*
makeletter :: Char -> [(Char,Int)]
makeletter x =hd[(a,b)\\a<-['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'] & b<-[1..17] | x==a || y==a]
//Start = makeletter 'a' 

maxii :: (String,Int) -> [Int]
maxii x = makeletter 
ufi :: [(String,Char)] [(String,Char)] Char -> [String]
ufi x = [fst3 a \\ (a,b)<-x ]
ufi x = [a\\a<-(map makeletter [snd3 a \\ a<-x])
*/
//Start = ufi [("hhhhh" ,'c')] [("hahahahaha",'h')] 'c' // []
//Start = ufi [("Good " ,'c'),("Happy" ,'z')] [("Friday",'h'),("Luck",'g')] 'a' // ["Good ","Luck"]
//Start = ufi [("May " ,'i'),("Happy " ,'z'),("must " ,'z'),("Happy" ,'i'),("end" ,'b')] [("Friday",'l'),("you ",'k'),("be " ,'x'),("Happier" ,'p'),("and" ,'a')] 'h' 
// ["May ","you ","be ","Happy"]


/*8. Blessing
It is better to end your words of blessing with an exclamation mark!
Given a list of strings,return a string containing all the substrings 
and ending with an ! character.
*/

blessing :: [String] -> String
blessing x = foldl (+++)"" x +++ "!"
//Start = blessing []  //"!"
//Start = blessing  ["Good ","Luck"] //"Good Luck!"
//Start = blessing  ["May ","you ","be ","Happy"] //"May you be Happy!"


/*9. Product
Given two lists of integers, compute the product of the largest 
integer from each list and the product of the smallest integer from each list. 
Return absolute value of these products as a tuple.

For the lists [1, 7, -9] and [3, 9, 5], the largest integers are 7 and 9,
respectively, and their product is 63. The smallest integers are -9 and 3, 
respectively, and their product is -27. The function should return the tuple (63, 27)
the absolute value of 63 and -27, are 63 and 27 respectively.
*/

productTuple :: [Int] [Int] -> (Int,Int)
productTuple x y = (abs(a*b),abs(c*d))
where 
	a = maxList x 
	b = maxList y 
	c = minList x 
	d = minList y
//Start = productTuple [1,7,-9] [3,9,5] // (63,27)
//Start = productTuple [-5,-9,-3] [2,4,8] // (24,18)
//Start = productTuple [1,3,9] [0,-2,-8] // (0,8)


/*10. Sums
Your input is list of tuples. Each tuple is containing 3 values: 
name of a customer, expenses of the customer and the Boolean value, showing if that 
that customer is married or not. For example: ("Anna", 300, True)
Write a function that will output sum of the expenses of all married customers.  
*/

sumAndMostFrequent :: [(String, Int, Bool)] -> Int
sumAndMostFrequent x = sum[b\\(a,b,c)<-x | c ==True]
//Start = sumAndMostFrequent [("Anna", 300, True), ("Bob", 150, False), ("Anna", 200, True), ("Charles", 100, True), ("Diana", 400, False), ("Eva", 250, False), ("Charles", 150, True)] //750
//Start = sumAndMostFrequent [("Greg", 220, True), ("Hilda", 80, False), ("Ian", 360, True), ("Julie", 290, True), ("Greg", 460, True), ("Karen", 130, False), ("Ian", 250, False)] // 1330
//Start = sumAndMostFrequent [("Liam", 110, False), ("Mia", 310, True), ("Noah", 210, True), ("Olivia", 140, False), ("Mia", 200, True), ("Noah", 90, False), ("Liam", 150, True)] // 870


/*11. Apply all
Given a list of integer and a list of function, apply each function to 
the given integer list and return the 2-Dimensional list.
	
[doubleAll, zeroAll, tripleAll] and [1,2,3,4,5] -> 
[[2,4,6,8,10],[0,0,0,0],[3,6,9,12,15]]
*/

falseAll ls = map (\x = False) ls
trueAll ls = map (\x = True) ls
trueFalse ls = [ result index \\ x <- ls & index <- [0..] ] where result index | isEven index = True = False

zeroStrAll ls = map (\x = "0") ls
oneStrAll ls = map (\x = "1") ls
falseStrAll ls = map (\x = "False") ls
trueStrAll ls = map (\x = "True") ls

applyAll :: [([a] -> [b])] [a] -> [[b]]
applyAll funcs x = [ a x \\a<-funcs]

//Start = applyAll [zeroStrAll, oneStrAll, falseStrAll] [1,2,3,4,5] 
// [["0","0","0","0","0"],["1","1","1","1","1"],["False","False","False","False","False"]]
//Start = applyAll [falseAll, trueAll] [] // [[],[]]
//Start = applyAll [] [1,2,3,4,5] // []
//Start = applyAll [] [] // []


/*12. Missing min
Given a list of non-negative integers, write a function which returns 
the smallest missing non-negative integer.

Input: [0,4,1,2] Output: 3
Input: [1,20] Output: 0
Input: [5,1,6,4,0,3,7] 	Output: 2
Input: [1,0,2,3] Output: 4
Input: [] Output: 0
*/


missingNumber :: [Int] -> Int
missingNumber x
| length x == 0 = 0
| length x == length t = maxList x + 1
= minList [ b \\ b<-t | (not(isMember b x))]
where 
	t = [(minList x)..(maxList x)]
	
//Start = missingNumber [] 										// 0
//Start = missingNumber [1,0,3,2] 								// 4
//Start = missingNumber [5,1,6,4,0,3,7] 						// 2
//Start = missingNumber [7,1,8,10,2,3,6,0,4,5] 					// 9
//Start = missingNumber [10,13,3,15,0,7,4,5,11,1,2,8,9,12,6] 	// 14


/*13. Max steps
Given three numbers a, b, and x, where a is the starting number, 
b is the ending number, and x is the multiplier (x > 1), 
find the maximum number of times you can multiply a by x before it exceeds b.

For a = 5, b = 10, and x = 2, the output should be 1, 
since 5*2 = 10, 1 is the largest power of 2 for 
the product to be less than or equal to 10.
	
For a = 1, b = 10, and x = 3, the output should be 2,
since 1*3*3 = 9, 2 is the largest power of 3 for 
the product to be less than or equal to 10.
*/

//maxMult :: Int Int Int -> Int

//Start = maxMult 5 10 2 // 1
//Start = maxMult 1 10 3 // 2
//Start = maxMult 4 50 3  // 2
//Start = maxMult 3 100 2 // 5
//Start = maxMult 40 2 3 // -1