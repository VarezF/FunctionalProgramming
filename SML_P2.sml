(*Freya Varez
 *2/10/2019
 *)
fun merge2 [] [] = []
  | merge2 [] L = L
  | merge2 L [] = L
  | merge2 (a::L1) (b::L2) = if a < b then (a::(merge2 L1 (b::L2))) else (b::(merge2 (a::L1) L2))

fun merge2Test() = 
    let      
        val merge2TestT1 = ((merge2 [2,5,6,8,9] [1,3,4,5,7,8,10]) = [1,2,3,4,5,5,6,7,8,8,9,10])   
        val merge2TestT2 = ((merge2 [1,2] [0,10,12]) = [0,1,2,10,12] ) 
        val merge2TestT3 = ((merge2 [1,3,3,5,5] [~1,2,4]) = [~1,1,2,3,3,4,5,5])
        val merge2TestT4 = ((merge2 [1,2,3] []) = [1,2,3])
        val merge2TestT5 = ((merge2 [][]) = [])
        val merge2TestT6 = ((merge2 [~3,~2,~1][1,2,3]) = [~3,~2,~1,1,2,3])
    in       
        print ("merge2Test:-------------------- \n" ^
        "test1: " ^ Bool.toString(merge2TestT1) ^ "\n" ^   
        "test2: " ^ Bool.toString(merge2TestT2) ^ "\n" ^  
        "test3: " ^ Bool.toString(merge2TestT3) ^ "\n" ^   
        "test4: " ^ Bool.toString(merge2TestT4) ^ "\n" ^
        "test5: " ^ Bool.toString(merge2TestT5) ^ "\n" ^
        "test6: " ^ Bool.toString(merge2TestT6) ^ "\n")   
    end 
val _ = merge2Test()

(*(b) merge2Tail
Re-write the merge2 function from part (a) as a tail-recursive function.  Name your function merge2Tail.  
(Hint: In your bases case(s), use revAppend  (which we defined in class) to add the reverse of the accumulated merged list to the other list.) 
The type of merge2Tail should be    int list -> int list -> int list.
*)
fun merge2Tail L1 L2 = 
    let
        fun revAppend [] L = L 
          | revAppend (x::rest) L = revAppend rest (x::L)
        fun merge [] [] acc = revAppend acc []
          | merge [] (a::L) acc = merge [] L (a::acc)
          | merge (a::L) [] acc = merge [] L (a::acc)
          | merge (a::L1) (b::L2) acc = if a < b then merge L1 (b::L2) (a::acc) else merge (a::L1) L2 (b::acc)
    in
        merge L1 L2 []
    end

fun merge2TailTest() = 
    let      
        val merge2TailTestT1 = ((merge2Tail [2,5,6,8,9] [1,3,4,5,7,8,10]) = [1,2,3,4,5,5,6,7,8,8,9,10])   
        val merge2TailTestT2 = ((merge2Tail [1,2] [0,10,12]) = [0,1,2,10,12] ) 
        val merge2TailTestT3 = ((merge2Tail [1,3,3,5,5] [~1,2,4]) = [~1,1,2,3,3,4,5,5])
        val merge2TailTestT4 = ((merge2Tail [1,2,3] []) = [1,2,3])
        val merge2TailTestT5 = ((merge2Tail [][]) = [])
        val merge2TailTestT6 = ((merge2Tail [~3,~2,~1][1,2,3]) = [~3,~2,~1,1,2,3])
    in       
        print ("merge2TailTest:-------------------- \n" ^
        "test1: " ^ Bool.toString(merge2TailTestT1) ^ "\n" ^   
        "test2: " ^ Bool.toString(merge2TailTestT2) ^ "\n" ^  
        "test3: " ^ Bool.toString(merge2TailTestT3) ^ "\n" ^   
        "test4: " ^ Bool.toString(merge2TailTestT4) ^ "\n" ^
        "test5: " ^ Bool.toString(merge2TailTestT5) ^ "\n" ^
        "test6: " ^ Bool.toString(merge2TailTestT6) ^ "\n")   
    end 
val _ = merge2TailTest()
(*(c) mergeN
Using merge2 function defined above and the fold function, define mergeN which takes a list of lists, 
each already in ascending order, and returns a new list containing all of the elements in sublists in ascending order. 
Provide an answer using fold; without using explicit recursion. 
The type of mergeN should be    int list list -> int list *)
fun mergeN [] = []
  | mergeN L =
  let
    fun merge2 [] [] = []
      | merge2 [] L = L
      | merge2 L [] = L
      | merge2 (a::L1) (b::L2) = if a < b then (a::(merge2 L1 (b::L2))) else (b::(merge2 (a::L1) L2))
    fun fold f base [] = base
      | fold f base (x::rest) = f x (fold f base rest)
  in
    fold (merge2) [] L
  end

fun mergeNTest() = 
    let 
        val mergeNTestT1 = ((mergeN [[1,2],[10,12],[2,5,6,8,9]]) = [1,2,2,5,6,8,9,10,12])   
        val mergeNTestT2 = ((mergeN [[3,4],[~3,~2,~1],[1,2,5,8,9]]) = [~3,~2,~1,1,2,3,4,5,8,9]) 
        val mergeNTestT3 = ((mergeN []) = [])
        val mergeNTestT4 = ((mergeN [[],[1],[2],[3]]) = [1,2,3])
        val mergeNTestT5 = ((mergeN [[1,2,3,4,5,6,7]]) = [1,2,3,4,5,6,7])
        val mergeNTestT6 = ((mergeN [[~3,~2,~1],[1,2,3]]) = [~3,~2,~1,1,2,3])
    in       
        print ("mergeNTest:-------------------- \n" ^
        "test1: " ^ Bool.toString(mergeNTestT1) ^ "\n" ^   
        "test2: " ^ Bool.toString(mergeNTestT2) ^ "\n" ^  
        "test3: " ^ Bool.toString(mergeNTestT3) ^ "\n" ^   
        "test4: " ^ Bool.toString(mergeNTestT4) ^ "\n" ^
        "test5: " ^ Bool.toString(mergeNTestT5) ^ "\n" ^
        "test6: " ^ Bool.toString(mergeNTestT6) ^ "\n")   
    end 
val _ = mergeNTest()

(*2. getInRange and countInRange***************************************************************************************************)
(*(a) getInRange
Define a function getInRange which takes two integer values, v1 and v2,  and a list L, 
and returns the values in L which are greater than v1 and less than v2 (exclusive).   
Your function shouldn’t need a recursion but should use a higher order function (map, fold, or filter).
The type of the getInRange function should be:   int -> int -> int list -> int list)
*)
fun getInRange v1 v2 L =
    let 
        fun filter pred [] = [] 
          | filter pred (x::rest) = if (pred x) then x::(filter pred rest) else (filter pred rest)
        fun between e = e > v1 andalso e < v2
    in
        filter (between) L
    end

fun getInRangeTest() = 
    let 
        val getInRangeTestT1 = ((getInRange  3 10 [1,2,3,4,5,6,7,8,9,10,11]) = [4,5,6,7,8,9])   
        val getInRangeTestT2 = ((getInRange ~5 5 [~10,~5,0,5,10]) = [0]) 
        val getInRangeTestT3 = ((getInRange ~1 1 [~2,2,3,4,5]) = [])
        val getInRangeTestT4 = ((getInRange 0 0 [1,2,3,4,5]) = [])
        val getInRangeTestT5 = ((getInRange 1 5 []) = [])
        val getInRangeTestT6 = ((getInRange 3 5 [~2,2,3,4,5]) = [4])
    in       
        print ("getInRangeTest:-------------------- \n" ^
        "test1: " ^ Bool.toString(getInRangeTestT1) ^ "\n" ^   
        "test2: " ^ Bool.toString(getInRangeTestT2) ^ "\n" ^  
        "test3: " ^ Bool.toString(getInRangeTestT3) ^ "\n" ^   
        "test4: " ^ Bool.toString(getInRangeTestT4) ^ "\n" ^
        "test5: " ^ Bool.toString(getInRangeTestT5) ^ "\n" ^
        "test6: " ^ Bool.toString(getInRangeTestT6) ^ "\n")   
    end 
val _ = getInRangeTest()

(*(b) countInRange123
Using getInRange function you defined in part(a) and without using explicit recursion, define a function countInRange 
which takes two integer values, v1 and v2,  and a nested list L, 
and returns the total number of values in L which are greater than v1 and less than v2 (exclusive).
The type of the countInRange  function should be:   int -> int -> int list list -> int)*)

fun countInRange v1 v2 L =
    let
        fun filter pred [] = [] 
          | filter pred (x::rest) = if (pred x) then x::(filter pred rest) else (filter pred rest)
        fun fold f base [] = base
          | fold f base (x::rest) = f x (fold f base rest)
        fun length [] = 0
          | length (a::L) = 1 + length L
        fun between e = e > v1 andalso e < v2
        fun cat L1 L2 = L1@L2
    in 
        length (filter (between) (fold (cat) [] L))
    end

fun countInRangeTest() = 
    let 
        val countInRangeTestT1 = ((countInRange 3 10 [[1,2,3,4],[5,6,7,8,9],[10,11]]) = 6)   
        val countInRangeTestT2 = ((countInRange ~5 5 [[~10,~5,~4],[0,4,5],[],[10]]) = 3) 
        val countInRangeTestT3 = ((countInRange 1 5 [[1,5],[1],[5],[]]) = 0)
        val countInRangeTestT4 = ((countInRange 0 0 [[1,2,3,5],[1,9],[5],[0]]) = 0)
        val countInRangeTestT5 = ((countInRange 1 5 []) = 0)
        val countInRangeTestT6 = ((countInRange 3 5 [[~2,2,3,4,5]]) = 1)
    in       
        print ("countInRangeTest:-------------------- \n" ^
        "test1: " ^ Bool.toString(countInRangeTestT1) ^ "\n" ^   
        "test2: " ^ Bool.toString(countInRangeTestT2) ^ "\n" ^  
        "test3: " ^ Bool.toString(countInRangeTestT3) ^ "\n" ^   
        "test4: " ^ Bool.toString(countInRangeTestT4) ^ "\n" ^
        "test5: " ^ Bool.toString(countInRangeTestT5) ^ "\n" ^
        "test6: " ^ Bool.toString(countInRangeTestT6) ^ "\n")   
    end 
val _ = countInRangeTest()

(*3. addLengths and addAllLengths***************************************************************************************)
datatype lengthUnit = INCH of int | FOOT of int | YARD of int
(*Define an ML function addLengths that takes two lengthUnit values  
and calculates the sum of those in INCH s. (Note that 1 foot = 12 inches  and 1 yard = 36 inches) 
The type of the addLengths function should be:   lengthUnit -> lengthUnit -> lengthUnit)*)

fun addLengths a b =
    let
        fun toInches (FOOT a) = a*12
          | toInches (YARD a) = a*36
          | toInches (INCH a) = a
    in
        INCH ((toInches a) + (toInches b))
    end

fun addLengthsTest() = 
    let 
        val addLengthsTestT1 = ((addLengths (FOOT 2) (INCH 5)) = INCH 29)
        val addLengthsTestT2 = ((addLengths (YARD 3) (INCH 3)) = INCH 111)
        val addLengthsTestT3 = ((addLengths (FOOT 3) (FOOT 5)) = INCH 96)
        val addLengthsTestT4 = ((addLengths (INCH 1) (INCH 1)) = INCH 2)
        val addLengthsTestT5 = ((addLengths (FOOT 3) (FOOT ~3)) = INCH 0)
        val addLengthsTestT6 = ((addLengths (YARD 3) (YARD 5)) = INCH 288)
    in       
        print ("addLengthsTest:-------------------- \n" ^
        "test1: " ^ Bool.toString(addLengthsTestT1) ^ "\n" ^   
        "test2: " ^ Bool.toString(addLengthsTestT2) ^ "\n" ^  
        "test3: " ^ Bool.toString(addLengthsTestT3) ^ "\n" ^   
        "test4: " ^ Bool.toString(addLengthsTestT4) ^ "\n" ^
        "test5: " ^ Bool.toString(addLengthsTestT5) ^ "\n" ^
        "test6: " ^ Bool.toString(addLengthsTestT6) ^ "\n")   
    end 
val _ = addLengthsTest()

(*(b)  addAllLengths:
define an ML function addAllLengths that takes a nested list of lengthUnit values 
and calculates the sum of those in INCH s. Your function shouldn’t need a recursion but should use functions “map” and “fold”. 
The type of the addAllLengths function should be:   lengthUnit list list -> lengthUnit*)
fun addAllLengths L = 
    let
        fun fold f base [] = base
          | fold f base (x::rest) = f x (fold f base rest)
        fun map f [] = [] 
          | map f (x::rest)  = (f x)::map f rest
        fun toInches (FOOT a) = a*12
          | toInches (YARD a) = a*36
          | toInches (INCH a) = a
        fun cat a b = a@b
        fun add a b = a+b
    in
        INCH (fold add 0 (map (toInches) (fold cat [] L)))
    end

fun addAllLengthsTest() = 
    let 
        val addAllLengthsTestT1 = ((addAllLengths [[YARD 2, FOOT 1], [YARD 1, FOOT 2, INCH 10],[YARD 3]]) = INCH 262 )
        val addAllLengthsTestT2 = ((addAllLengths [[YARD 3], [INCH 3]]) = INCH 111)
        val addAllLengthsTestT3 = ((addAllLengths [[FOOT 2], [FOOT 2, INCH 2],[]]) = INCH 50)
        val addAllLengthsTestT4 = ((addAllLengths []) = INCH 0)
        val addAllLengthsTestT5 = ((addAllLengths [[FOOT 3],[FOOT ~3]]) = INCH 0)
        val addAllLengthsTestT6 = ((addAllLengths [[YARD 3],[YARD 5, FOOT 0]] = INCH 288))
    in       
        print ("addAllLengthsTest:-------------------- \n" ^
        "test1: " ^ Bool.toString(addAllLengthsTestT1) ^ "\n" ^   
        "test2: " ^ Bool.toString(addAllLengthsTestT2) ^ "\n" ^  
        "test3: " ^ Bool.toString(addAllLengthsTestT3) ^ "\n" ^   
        "test4: " ^ Bool.toString(addAllLengthsTestT4) ^ "\n" ^
        "test5: " ^ Bool.toString(addAllLengthsTestT5) ^ "\n" ^
        "test6: " ^ Bool.toString(addAllLengthsTestT6) ^ "\n")   
    end 
val _ = addAllLengthsTest()
(*4.  sumTree and createSumTree********************************************************************************************************)
datatype 'a Tree = LEAF of 'a | NODE of 'a * 'a Tree * 'a Tree
(*(a) sumTree
Write a function sumTree that takes a tree of type int Tree and calculates the sum of the values stored in the leaves only.  
The type of the sumTree function should be: int Tree -> int   *)
fun sumTree (LEAF L) = L
  | sumTree (NODE (a, b, c)) = (sumTree b) + (sumTree c) 

fun sumTreeTest() = 
    let 
        val t1 = NODE (1,NODE(2, NODE (3,LEAF 4, LEAF 5), LEAF 6),NODE(7, LEAF 8, LEAF 9))
        val sumTreeTestT1 = ((sumTree t1) = 32)

        val t2 = NODE (0,NODE(0, LEAF 4, NODE (0,LEAF 8, LEAF 9)),NODE(0, NODE(0,LEAF 10, NODE (0, LEAF 12, LEAF 13)), LEAF 7))
        val sumTreeTestT2 = ((sumTree t2) = 63)

        val sumTreeTestT3 = (sumTree (LEAF 3) = 3)

        val t4 = NODE (1,NODE(2, NODE (3,LEAF 0, LEAF 0), LEAF 0),NODE(7, LEAF 0, LEAF 0))
        val sumTreeTestT4 = ((sumTree t4) = 0)

        val t5 = NODE (1,NODE(2, NODE (3,LEAF ~1, LEAF 1), LEAF ~1),NODE(7, LEAF 2, LEAF ~1))
        val sumTreeTestT5 = ((sumTree t5) = 0)

        val sumTreeTestT6 = (sumTree (NODE (3, LEAF 2, LEAF 3)) = 5)
    in       
        print ("sumTreeTest:-------------------- \n" ^
        "test1: " ^ Bool.toString(sumTreeTestT1) ^ "\n" ^   
        "test2: " ^ Bool.toString(sumTreeTestT2) ^ "\n" ^  
        "test3: " ^ Bool.toString(sumTreeTestT3) ^ "\n" ^   
        "test4: " ^ Bool.toString(sumTreeTestT4) ^ "\n" ^
        "test5: " ^ Bool.toString(sumTreeTestT5) ^ "\n" ^
        "test6: " ^ Bool.toString(sumTreeTestT6) ^ "\n")   
    end 
val _ = sumTreeTest()

(*(b) createSumTree:
Write a function createSumTree takes an  int Tree  value and returns an int Tree 
where the interior nodes store the sum of the leaf values underneath them. 
The type of the createSumTree function should be int Tree -> int Tree )   *)
fun createSumTree (LEAF L) = (LEAF L)
  | createSumTree (NODE (a, b, c)) =
    let
        fun sumTree (LEAF L) = L
          | sumTree (NODE (a, b, c)) = (sumTree b) + (sumTree c)
    in
        NODE(sumTree b + sumTree c, createSumTree b, createSumTree c)
    end

 fun createSumTreeTest() = 
    let 
        val t1 = NODE (0,NODE(0, NODE (0, LEAF 4, LEAF 5), LEAF 6),NODE(0, LEAF 8, LEAF 9))
        val createSumTreeTestT1 = ((createSumTree t1) = (NODE (32, NODE(15, NODE (9, LEAF 4, LEAF 5), LEAF 6), NODE(17, LEAF 8, LEAF 9))))

        val t2 = NODE (0, NODE (0, NODE (0,LEAF 4,LEAF 5),LEAF 6),NODE (0, LEAF 8, LEAF 9))
        val createSumTreeTestT2 = ((createSumTree t2) = (NODE (32, NODE (15,NODE (9,LEAF 4,LEAF 5),LEAF 6), NODE (17,LEAF 8,LEAF 9))))

        val createSumTreeTestT3 = (createSumTree (LEAF 3) = LEAF 3)

        val t4 = NODE (0, NODE(0, NODE (0,LEAF 0, LEAF 0), LEAF 0),NODE(0, LEAF 0, LEAF 0))
        val createSumTreeTestT4 = ((createSumTree t4) = (NODE (0, NODE(0, NODE (0,LEAF 0, LEAF 0), LEAF 0),NODE(0, LEAF 0, LEAF 0))))

        val t5 = NODE (1, NODE(2, NODE (3,LEAF ~1, LEAF 1), LEAF ~1), NODE(7, LEAF 2, LEAF ~1))
        val createSumTreeTestT5 = ((createSumTree t5) = (NODE (0, NODE(~1, NODE (0,LEAF ~1, LEAF 1), LEAF ~1), NODE(1, LEAF 2, LEAF ~1))))

        val createSumTreeTestT6 = (createSumTree (NODE (3, LEAF 2, LEAF 3)) = (NODE (5, LEAF 2, LEAF 3)))
    in       
        print ("createSumTreeTest:-------------------- \n" ^
        "test1: " ^ Bool.toString(createSumTreeTestT1) ^ "\n" ^   
        "test2: " ^ Bool.toString(createSumTreeTestT2) ^ "\n" ^  
        "test3: " ^ Bool.toString(createSumTreeTestT3) ^ "\n" ^   
        "test4: " ^ Bool.toString(createSumTreeTestT4) ^ "\n" ^
        "test5: " ^ Bool.toString(createSumTreeTestT5) ^ "\n" ^
        "test6: " ^ Bool.toString(createSumTreeTestT6) ^ "\n")   
    end 
val _ = createSumTreeTest()

(*5.  foldListTree ******************************************************************************************************
Write a function foldListTree that takes a function (f), a base value (base), and a listTree (t) 
and combines the values in the lists of the leaf notes in tree t by applying function f. 
(The leaves of the tree are scanned from left to right).  
foldListTree is invoked as:   foldListTree f base t  where f is the combining function of type 'a ->'a ->'a. 
The type of foldListTree should be:  ('a -> 'a -> 'a) -> 'a -> 'a listTree -> 'a *)
datatype 'a listTree = listLEAF of ('a list) | listNODE of ('a listTree list)

fun foldListTree f base (listLEAF t) = 
    let
        fun fold f base [] = base
          | fold f base (x::rest) = f x (fold f base rest)
    in
        fold f base t
    end
  | foldListTree f base (listNODE a) =
    let
        fun map f [] = [] 
          | map f (x::rest)  = (f x)::map f rest
        fun fold f base [] = base
          | fold f base (x::rest) = f x (fold f base rest)
    in
        fold f base (map (foldListTree f base) a)
    end

 fun foldListTreeTest() = 
    let 
        fun concat a b = a^b
        val L1 = listLEAF ["School","-","of","-","Electrical"] 
        val foldListTreeTestT1 = ((foldListTree concat "" L1) = "School-of-Electrical")
       
        val L2 = listLEAF ["-","Engineering","-"] 
        val foldListTreeTestT2 = ((foldListTree concat "" L2) = "-Engineering-")
       
        val L3 = listLEAF ["and","-","Computer","-"] 
        val foldListTreeTestT3 = ((foldListTree concat "" L3) = "and-Computer-")
        
        val L4 = listLEAF ["Science"] 
        val foldListTreeTestT4 = ((foldListTree concat "" L4) = "Science")
        
        val L5 = listLEAF ["-WSU"] 
        val foldListTreeTestT5 = ((foldListTree concat "" L5) = "-WSU")
        
        val N1 = listNODE [L1,L2] 
        val foldListTreeTestT6 = ((foldListTree concat "" N1) = "School-of-Electrical-Engineering-")
        
        val N2 = listNODE [N1,L3]
        val foldListTreeTestT7 = ((foldListTree concat "" N2) = "School-of-Electrical-Engineering-and-Computer-")
        
        val t5 = listNODE [N2,L4,L5]
        val foldListTreeTestT8 = ((foldListTree concat "" t5) = "School-of-Electrical-Engineering-and-Computer-Science-WSU")

        fun add a b = a + b
        val b1 = listNODE[listLEAF [1], listLEAF [2]]
        val foldListTreeTestT9 = ((foldListTree add 0 b1) = 3)

        val b2 = listNODE[b1, listLEAF[5]]
        val foldListTreeTestT10 = ((foldListTree add 0 b2) = 8)
    in       
        print ("foldListTreeTest:-------------------- \n" ^
        "test1: " ^ Bool.toString(foldListTreeTestT1) ^ "\n" ^   
        "test2: " ^ Bool.toString(foldListTreeTestT2) ^ "\n" ^  
        "test3: " ^ Bool.toString(foldListTreeTestT3) ^ "\n" ^   
        "test4: " ^ Bool.toString(foldListTreeTestT4) ^ "\n" ^
        "test5: " ^ Bool.toString(foldListTreeTestT5) ^ "\n" ^
        "test6: " ^ Bool.toString(foldListTreeTestT6) ^ "\n" ^
        "test7: " ^ Bool.toString(foldListTreeTestT7) ^ "\n" ^
        "test8: " ^ Bool.toString(foldListTreeTestT8) ^ "\n" ^
        "test9: " ^ Bool.toString(foldListTreeTestT9) ^ "\n" ^
        "test10: " ^ Bool.toString(foldListTreeTestT10) ^ "\n")   
    end 
val _ = foldListTreeTest()