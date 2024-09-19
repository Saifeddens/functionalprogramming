module enda
import StdEnv

/* Write <Saifeddin Alkurdi> and <o9ffdc> here.
by this YOU DECLARE this file is 
YOUR OWN SOLUTION for functional 
programming endterm 2024 May 15.
If name and neptun missing, 
we will not check the file!!!*/


// Ex 6 is of 20 points, all other of 10 points.


/*1. Zeros
You are given an integer array, get all 0's and move them to the end 
of array without changing relative order of the non-zero elements.
*/

moveZeroes :: {Int} -> {Int}
moveZeroes list1 = {a\\a<-(y++yzero)}
where
	y = [a\\a<-:list1| a<>0]
	yzero = [a\\a<-:list1 | a==0]
	
//Start = moveZeroes {0, 0, 3, 0, 0, 1, 0, 9, 0, 0, 0}//{3,1,9,0,0,0,0,0,0,0,0}
//Start = moveZeroes {4, 0, 5, 0, 6, 7, 0}//{4,5,6,7,0,0,0}
//Start = moveZeroes {0,0,0,0,0,0}//{0,0,0,0,0,0}
//Start = moveZeroes {1,2,3,4,5}//{1,2,3,4,5}



/*2. Max occurence
Implement a function that takes a string as input and determines the character 
that has the highest frequency. The function should return a tuple containing
the character with the highest frequency and its count. 
If two or more characters share the highest frequency, return all of them.
*/
countocc :: Char String -> Int
countocc letter word = sum[1\\a<-:word | (toChar a)==letter]

highest :: String -> Int
highest word = last(sort(removeDup[countocc (toChar a) word \\a<-:word ]))

//Start = countocc 'c' "helloc"
maxCharFrequency  :: String -> [(Char,Int)]
maxCharFrequency word = (removeDup[(toChar a , countocc (toChar a) word)\\a<-:word | (countocc (toChar a) word) == highest word])

//Start = maxCharFrequency "apples" // [('p',2)]
//Start = maxCharFrequency "mississippi" // [('i',4),('s',4)]
//Start = maxCharFrequency "committee" // [('m',2),('t',2),('e',2)]



//Record related tasks

:: Person = {
            name :: String,
            birthday :: (Int, Int, Int)
            }

:: RentalStatus = Rented | Available 

:: Apartment = {
                    owner :: Person,
                    rentalStatus :: RentalStatus,
                    numberOfRooms :: Int,
                    floor :: Int
               }

:: House = {
                apartments :: {Apartment},
                location :: String,
                numberOfFloors :: Int
            }

p1 :: Person 
p1 = {name = "Abdullah", birthday = (2002, 8, 28)}
p2 :: Person 
p2 = {name = "Ali", birthday = (2009, 5, 28)}
p3 :: Person 
p3 = {name = "Duaa", birthday = (2003, 2, 25)}
p4 :: Person 
p4 = {name = "Tamas", birthday = (2000, 9, 02)}

ap1 :: Apartment
ap1 = {owner = p1, rentalStatus = Rented, numberOfRooms = 3, floor = 1}
ap2 :: Apartment
ap2 = {owner = p2, rentalStatus = Rented, numberOfRooms = 5, floor = 2}
ap3 :: Apartment
ap3 = {owner = p3, rentalStatus = Available, numberOfRooms = 4, floor = 1}
ap4 :: Apartment
ap4 = {owner = p4, rentalStatus = Available, numberOfRooms = 1, floor = 2}
ap5 :: Apartment
ap5 = {owner = p1, rentalStatus = Rented, numberOfRooms = 7, floor = 3}

h1 :: House
h1 = {apartments = {ap1, ap2, ap3, ap4, ap5}, location = "Budapest", numberOfFloors = 4}

h2 :: House
h2 = {apartments = {ap1, ap2}, location = "Gyor", numberOfFloors = 2}

h3 :: House
h3 = {apartments = {ap1, ap4, ap5}, location = "Budapest", numberOfFloors = 3}

h22 :: House
h22 = {apartments = {ap1, ap2, ap3, ap4, ap5}, location = "Gyor", numberOfFloors = 2}

h32 :: House
h32 = {apartments = {ap1, ap2}, location = "Budapest", numberOfFloors = 4}

h42 :: House
h42 = {apartments = {ap3, ap4}, location = "Gyor", numberOfFloors = 2}



/*3. Kids
Find out if the given house has an apartment, who's owner is younger than 18 (year <= 2005).
*/

isThereKids :: House -> Bool
isThereKids house = or[fst3(a.owner.birthday) >= 2005 \\a<-:house.apartments]
//Start = isThereKids h1 // True
//Start = isThereKids h2 // True
//Start = isThereKids h3 // False



/*4. Available houses
Calculate the number of "Available" apartments in a given House.
*/
instance == RentalStatus
where
	(==) Rented Rented = True
	(==) Available Available =True
	(==) _ _ =False
	
calcAvailable :: House -> Int 
calcAvailable house = sum[1\\a<-:house.apartments | a.rentalStatus == Available ]

//Start = calcAvailable h1 // 2
//Start = calcAvailable h2 // 0
//Start = calcAvailable h3 // 1



/*5. Yield 
Create enumeration type called Yield that has 2 possible values High or Low. 
Your input is house and you should determine if that house gives high yield or low.
House is High if it has more than 1 floor, more than 2 apartments and more than 
half of its apartments are rented out. Otherwise, it is Low.
*/

:: Yield = High | Low
/*
determineYield :: House -> Yield
determineYield house 
| ((hd[a.numberOfFloors\\a<-:house])>1) && ((sum[1\\a<-:house.apartments]) > 2) && ( (sum[1\\a<-:house.apartments | a.rentalStatus == Rented]) > ((sum[1\\a<-:house.apartments])/2) ) = High
=Low
*/

//Start = determineYield h1//High
//Start = determineYield h22//High
//Start = determineYield h32//Low
//Start = determineYield h42//Low



/*6. Equal houses - 20 points
Your task is to compare if two houses are equal or not. Each house has array of apartments. 
Two houses are equal, if every apartment is identical in given houses. Consider that 
both houses have the same number of apartments. Two apartments are identical if they 
are on same floor and have the same amount of room numbers, have the same owner and 
the same rental status. Two persons are equal if names and birthdates (all components)
are equal. Create instances for == operator for Person (5p), Apartment (5p),
RentalStatus (5p) and House (5p) types.
*/
instance == Person 
where
	(==) p1 p2 = p1.name == p2.name


//rentalstatus instance == is previously written
/*
instance == House 
where
	(==) house1 house2 = func house1 house2 
	
fourth :: (a,b,c,d) -> d
fourth (_,_,_,x) = x


func :: House House -> Bool
func house1 house2 = and[(fst3 x == fst3 y) && (snd3 x == snd3 y) && (thd3 x == thd3 y) \\x<-c , y<-b | x.owner == y.owner]
where
	 c = [(a.floor , a.numberOfRooms , a.rentalStatus )\\a<-:house1.apartments]  
	 b = [(a.floor , a.numberOfRooms , a.rentalStatus )\\a<-:house2.apartments] 
*/
//Start = h1==h22 // True
//Start = h22==h32 // True
//Start = h32==h42// False



/*7. Apartment sort
Given a list of apartments sort the apartments in increasing order of rentability.
Use built-in sort and create instance of < operator for Apartment.
One apartment is more rentable than the other: 
if it is at a higher floor than the other,
if the floors are same then,
it has more rooms than the other.
*/

// instance comes here
	
//Start = sort [ap3,ap2,ap4,ap1] 
// [(Apartment (Person "Abdullah" (2002,8,28)) Rented 3 1),(Apartment (Person "Duaa" (2003,2,25)) Available 4 1),(Apartment (Person "Tamas" (2000,9,2)) Available 1 2),(Apartment (Person "Ali" (2009,5,28)) Rented 5 2)]



/*8. Max branch sum
You are given a tree. Implement a function that returns the maximum sum of a route from root to a leaf.
For example, given the following tree:

    17
   /  \
  3   -10
 /    /  \
2    16   1
         /
        13

The function should return 23, since 17 -> -10 -> 16 is the route from root to leaf with the maximum sum.
*/

:: Tree a = Node a (Tree a) (Tree a) | Leaf

t1 = Node 5 (Node -22 (Node 9 Leaf Leaf) (Node 50 Leaf Leaf)) (Node 11 (Node 9 Leaf Leaf) (Node 2 Leaf Leaf))
t2 = Node 6 (Node 50 (Node -100 Leaf Leaf) (Node -10 Leaf Leaf)) (Node 40 Leaf Leaf)
t3 = Node 17 (Node 3 (Node 2 Leaf Leaf) Leaf) (Node -10 (Node 16 Leaf Leaf) (Node 1 (Node 13 Leaf Leaf) Leaf))
t4 = Node 5 (Node 4 (Node -80 Leaf Leaf) (Node -60 Leaf Leaf)) (Node -10 (Node -90 Leaf Leaf) (Node -100 Leaf Leaf))

getnode :: (Tree Int) -> Int
getnode Leaf = 0
getnode (Node x le ri) = x

maxSum :: (Tree Int) -> Int
maxSum Leaf = 0
maxSum (Node x le ri) 
| abs(getnode le) > abs(getnode ri) = x + ((maxSum le)) 
= x +  (maxSum ri)

//here i ddi an absolute value on the values becasue 
//Start = maxSum t1 // 33
//Start = maxSum (Node 17 Leaf Leaf) // 17
//Start = maxSum t2 // 46
//Start = maxSum t3 // 23
//Start = maxSum t4 // -51



/*9. Moving in tree
Given a string with moves to make and a Tree of strings write a function that
goes through the tree in the order of moves specified and returns a string.
Please put a space after every string. If you reach a leaf and still have 
moves left return the sentence up to that leaf.
*/

t1s :: Tree String 
t1s = Node "Hi" (Node "You" (Node "are nice. :)" Leaf Leaf) (Node "are smart. :)" Leaf Leaf) ) (Node "I" (Node "am Happy. :)" Leaf Leaf) (Node "am Sad. :(" Leaf Leaf) )


f2 :: (Tree String) String -> String
f2 Leaf str = ""
f2 (Node x le ri) str
| hd[a\\a<-y] == 82 = x +++ f2 ri (toString[a\\a<-tl(hd[a\\a<-y]) ])
= x +++ f2 le (tl(hd[a\\a<-y]) )
where
	y = [toInt(toChar a)\\a<-:str]

//Start = toInt 'L'
Start = f2 t1s "LLL"  // "Hi You are nice. :) "  // WE take "Hi" from root node , then go left and take "You" , then go left again and take "are nice. :)"
//Start = f2 t1s "LRLLL"  // "Hi You are smart. :) "
//Start = f2 t1s "RRRLR"   // "Hi I am Sad. :( "



/*10. Valid time
Task1 define type Time. Time is a type synonym for a tuple, a pair of two integers having hour, minute.
Task2 Write a function that takes a Time and checks if the Time is a valid time.
One day has 24 hours and one hour has 60 minutes.
*/

// Time type come here
:: Time :== (Int,Int)

d1 :: Time
d1 = (12,13)
d2 :: Time
d2 = (1,61) 
d3 :: Time
d3 = (25,0)
d4 :: Time
d4 = (-1,0) 

ValidTime :: Time -> Bool
ValidTime (a,b) = a >= 0 && a<=24 && b >= 0 && b <= 60

//Start = ValidTime d1 //True
//Start = ValidTime d2 //False
//Start = ValidTime d3 //False
//Start = ValidTime d4 //False