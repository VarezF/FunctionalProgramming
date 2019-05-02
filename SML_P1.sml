
fun exists (x, []) = false
  | exists (x, (y::t)) = if x = y then true else exists (x, t)

fun listUnion t i = 
    let
        fun removeDuplicates [] = []
        | removeDuplicates (a::b) = if exists (a, b) then removeDuplicates b else a::removeDuplicates b
        fun exists (x, []) = false
          | exists (x, (y::t)) = if x = y then true else exists (x, t)
    in
        removeDuplicates (t@i)
    end

fun replace n v (a::L) =
    let fun sizeOf [] = 0
    |   sizeOf (a::L) = 1 + sizeOf L
    in 
        if sizeOf (a::L) < n then a::L else 
        let
            fun validReplace n v (a::L) = if n = 0 then v::L else a::(validReplace (n - 1) v L)
            |   validReplace n v [] = [] (* <- won't ever be evaluated, but compiler gets angry if it's not here... (nonexhaustive)*)
        in 
            validReplace n v (a::L) 
        end
    end
 | replace n v [] = []

fun prereqFor ([], s) = []
  | prereqFor ((l::L), s) =
    let  
        val (a,b) = l 
        fun exists (x, []) = false
          | exists (x, (y::t)) = if x = y then true else exists (x, t)
    in 
        if exists (s,b) then a::prereqFor(L, s) else prereqFor (L, s)
    end

fun isPalindrome "" = true
  | isPalindrome s = 
    let 
        val r = List.rev(String.explode(s))
        val s = String.explode(s)
        fun sameList [] [] = true
          | sameList (#" "::s) (b::r) = sameList s (b::r) 
          | sameList (a::s) (#" "::r) = sameList (a::s) r
          | sameList (a::s) (b::r) = if Char.toUpper(a) = Char.toUpper(b) then sameList s r else false
    in 
        sameList s r
    end

fun groupSumtoN N [] = [[]]
  | groupSumtoN N L = 
    let 
        fun findSub n (l::L) = if n < l then 
            (if l > N then [l]::(findSub N L) else []::(findSub N (l::L)))
            else 
             let
                fun sum (l::L) = l + sum L 
                  | sum [] = 0 
             in 
                (case (findSub(n-l) L) of (a::b) => if sum (l::a) > N then [l]::(a::b) else (l::a)::b 
                | [] => [[l]])
             end
          | findSub n [] = []
    in
        findSub N L
    end

(*_____________________________________________________Test Cases______________________________________________*)

fun existsTest () =    
    let       
        val existsT1 = (exists(8,[7]) = false )
        val existsT2 = (exists("one",["two","one"]) = true )
        val existsT3 = (exists(true,[false,false]) = false )
        val existsT4 = (exists(1,[]) = false )
        val existsT5 = (exists(1,[1,2,3]) = true )
        val existsT6 = (exists([1],[[1]]) = true )
        val existsT7 = (exists([1],[[3],[5]]) = false )
        val existsT8 = (exists("c",["b","c","z"]) = true )
    in       
        print ("\n------------- \nexists:\n" ^              
        "test1: " ^ Bool.toString(existsT1) ^ "\n" ^             
        "test2: " ^ Bool.toString(existsT2) ^ "\n" ^       
        "test3: " ^ Bool.toString(existsT3) ^ "\n" ^
        "test4: " ^ Bool.toString(existsT4) ^ "\n" ^             
        "test5: " ^ Bool.toString(existsT5) ^ "\n" ^       
        "test6: " ^ Bool.toString(existsT6) ^ "\n" ^
        "test7: " ^ Bool.toString(existsT7) ^ "\n" ^
        "test8: " ^ Bool.toString(existsT8) ^ "\n")
end 
val _ = existsTest()

fun listUnionTest () =    
    let       
        val ListUnionT1 = (listUnion [1,3,4] [2,3,4,5] = [1,2,3,4,5] )
        val ListUnionT2 = (listUnion [1,1,2,3,3,3] [1,3,4,5] = [2,1,3,4,5])
        val ListUnionT3 = (listUnion ["a","b","c"] ["b","b","d"] = ["a","c","b","d"])
        val ListUnionT4 = (listUnion [[1,2],[2,3]] [[1],[2,3],[2,3]] = [[1,2],[1],[2,3]])
        val ListUnionT5 = (listUnion [] [] = [])
        val ListUnionT6 = (listUnion [] ["a","b","b"] = ["a","b"])
    in       
        print ("\n------------- \nlistUnion:\n" ^              
        "test1: " ^ Bool.toString(ListUnionT1) ^ "\n" ^             
        "test2: " ^ Bool.toString(ListUnionT2) ^ "\n" ^       
        "test3: " ^ Bool.toString(ListUnionT3) ^ "\n" ^
        "test4: " ^ Bool.toString(ListUnionT4) ^ "\n" ^             
        "test5: " ^ Bool.toString(ListUnionT5) ^ "\n" ^       
        "test6: " ^ Bool.toString(ListUnionT6) ^ "\n")
end 
val _ = listUnionTest();

fun replaceTest () =    
    let       
        val replaceT1 = (replace 3 40 [1, 2, 3, 4, 5, 6] = [1,2,3,40,5,6])
        val replaceT2 = (replace 0 "X" ["a", "b", "c", "d"] = ["X","b","c","d"]) 
        val replaceT3 = (replace 4 false [true, false, true, true, true] = [true,false,true,true,false]) 
        val replaceT4 = (replace 5 6 [1,2,3,4,5] = [1,2,3,4,5]) 
        val replaceT5 = (replace 6 7 [1,2,3,4,5] = [1,2,3,4,5]) 
        val replaceT6 = (replace 3 5 [] = []) 
        val replaceT7 = (replace 20 0 [1, 2, 3, 4, 5, 6] = [1, 2, 3, 4, 5, 6]) 
    in       
        print ("\n------------- \nreplace:\n" ^              
        "test1: " ^ Bool.toString(replaceT1) ^ "\n" ^             
        "test2: " ^ Bool.toString(replaceT2) ^ "\n" ^       
        "test3: " ^ Bool.toString(replaceT3) ^ "\n" ^
        "test4: " ^ Bool.toString(replaceT4) ^ "\n" ^             
        "test5: " ^ Bool.toString(replaceT5) ^ "\n" ^  
        "test6: " ^ Bool.toString(replaceT6) ^ "\n" ^      
        "test7: " ^ Bool.toString(replaceT7) ^ "\n")
end 
val _ = replaceTest()

fun prereqForTest () =
   let 
     val prereqsList = [("Cpts122",["CptS121"]), ("CptS132",["CptS131"]), ("CptS223",["CptS122", "MATH216"]), ("CptS233",["CptS132", "MATH216"]), ("CptS260",["CptS223", "CptS233"]), 
                       ("CptS315",["CptS223", "CptS233"]), ("CptS317",["CptS122", "CptS132", "MATH216"]), ("CptS321",["CptS223", "CptS233"]), ("CptS322",["CptS223","CptS233"]), 
                       ("CptS350",["CptS223","CptS233", "CptS317"]), ("CptS355",["CptS223"]), ("CptS360",["CptS223","CptS260"]),("CptS370",["CptS233","CptS260"]),
                       ("CptS427",["CptS223","CptS360", "CptS370", "MATH216", "EE234"])]
     val prereqForT1 = (prereqFor (prereqsList,"CptS260") = ["CptS360","CptS370"] )
     val prereqForT2 = (prereqFor (prereqsList,"CptS223") = ["CptS260","CptS315","CptS321","CptS322","CptS350","CptS355","CptS360","CptS427"] )
     val prereqForT3 = (prereqFor (prereqsList,"CptS355") = [] )
     val prereqForT4 = (prereqFor (prereqsList,"") = [] )
     val prereqForT5 = (prereqFor (prereqsList,"Math101") = [])
   in 
     print ("\n------------- \nprereqFor:\n" ^ 
            "  test1: " ^ Bool.toString(prereqForT1) ^ "\n" ^
            "  test2: " ^ Bool.toString(prereqForT2) ^ "\n" ^
            "  test3: " ^ Bool.toString(prereqForT3) ^ "\n" ^
            "  test4: " ^ Bool.toString(prereqForT4) ^ "\n" ^
 	        "  test5: " ^ Bool.toString(prereqForT5) ^ "\n")		
   end
val _ = prereqForTest()


fun isPalindromeTest () =    
    let       
        val isPalindromeT1 = (isPalindrome "a01 02 2010A" = true)
        val isPalindromeT2 = (isPalindrome "Doc note I dissent a fast never prevents a fatness I diet on cod" = true )
        val isPalindromeT3 = (isPalindrome "Yreka Bakery"= true )
        val isPalindromeT4 = (isPalindrome "top cart pop tracPOT"= true )
        val isPalindromeT5 = (isPalindrome "this" = false )
        val isPalindromeT6 = (isPalindrome "Hello World!"= false )
        val isPalindromeT7 = (isPalindrome "123456"= false )
        val isPalindromeT8 = (isPalindrome ""= true )
    in       
        print ("\n------------- \nisPalindrome:\n" ^              
        "test1: " ^ Bool.toString(isPalindromeT1) ^ "\n" ^             
        "test2: " ^ Bool.toString(isPalindromeT2) ^ "\n" ^       
        "test3: " ^ Bool.toString(isPalindromeT3) ^ "\n" ^
        "test4: " ^ Bool.toString(isPalindromeT4) ^ "\n" ^             
        "test5: " ^ Bool.toString(isPalindromeT5) ^ "\n" ^       
        "test6: " ^ Bool.toString(isPalindromeT6) ^ "\n" ^
        "test7: " ^ Bool.toString(isPalindromeT7) ^ "\n" ^
        "test8: " ^ Bool.toString(isPalindromeT8) ^ "\n")
end 
val _ = isPalindromeTest ()

fun groupSumtoNTest () =    
    let       
        val groupSumtoNT1 = (groupSumtoN 15 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = [[1,2,3,4,5],[6,7],[8],[9],[10]])
        val groupSumtoNT2 = (groupSumtoN 11 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = [[1,2,3,4],[5,6],[7],[8],[9],[10]])
        val groupSumtoNT3 = (groupSumtoN 1 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = [[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]])
        val groupSumtoNT4 = (groupSumtoN 5 [] = [[]]) 
        val groupSumtoNT5 = (groupSumtoN 0 [] = [[]]) 
        val groupSumtoNT6 = (groupSumtoN 100 [1,12,5,4,8,13,20]= [[1,12,5,4,8,13,20]])
        val groupSumtoNT7 = (groupSumtoN 10 [1,12,5,4,8,13,20] = [[1],[12],[5,4],[8],[13],[20]])
    in       
        print ("\n------------- \ngroupSumtoNTest\n" ^              
        "test1: " ^ Bool.toString(groupSumtoNT1) ^ "\n" ^             
        "test2: " ^ Bool.toString(groupSumtoNT2) ^ "\n" ^       
        "test3: " ^ Bool.toString(groupSumtoNT3) ^ "\n" ^
        "test4: " ^ Bool.toString(groupSumtoNT4) ^ "\n" ^             
        "test5: " ^ Bool.toString(groupSumtoNT5) ^ "\n" ^       
        "test6: " ^ Bool.toString(groupSumtoNT6) ^ "\n" ^
        "test7: " ^ Bool.toString(groupSumtoNT7) ^ "\n")
    end 
val _ = groupSumtoNTest ()