(*Problem 1*)
  datatype option = NONE | SOME of int;
  fun optionMax(NONE, NONE) = NONE | optionMax(SOME(m),NONE) = SOME(m)
| optionMax(NONE, SOME(m)) = SOME(m) | optionMax(SOME(m), SOME(n)) = if m >= n then SOME(m) else SOME(n);

(*Problem 2 assumimg parameters are already ordered*)
  fun setAddElem(nil, y) = [y] | setAddElem(x::xs, y) = if y < x then y::x::xs
  else if y = x then x::xs else x::setAddElem(xs,y);

    fun setUnion(nil, ys) = ys | setUnion(xs, nil) = xs |
    setUnion(x::xs,y::ys) = if x < y then x::setUnion(xs,y::ys)
                            else if x = y then x::setUnion(xs, ys)
                            else y::setUnion(x::xs,ys);


    fun setIntersection(nil, ys) = nil | setIntersection(xs,nil)=nil |setIntersection(x::xs, y::ys) = if x = y then x::setIntersection(xs,ys) else if x < y then setIntersection(xs,y::ys) else setIntersection(x::xs,ys); 

    fun setMax(xs) = List.last(xs);

(*Problem 3*)
(*a. {b}U A basically gives us all the elements in A plus b since b is not an element of A. Adding one more element into set A means that the powerset of A expands by inserting b in every set in the powerset of A (including inserting b into the empty set that results in {b}). Therefore, if there are x elements in powerset A, there will be an additional x elements in powerset of union A and the set containing b. Therefore, cardinality of powerset of union A and the set containing b is two times of the cardinality of powerset A. 
 b. If we start from when A is an empty set. $\mid\$A$\mid\$ = 0, P(A) = {emptySet}, and  $\mid\$
 *)

(*Problem 4*)
(*a. P(P({1,2})) = P({{emptySet},{1},{2},{1,2}}) = {emptySet,{E},{1},{2},{1,2}
                                                {{E},{1}},{{E},{2}},{{E},{1,2}},{{1},{2}},{{1},{1,2}},{{2},{1,2}},
                                                {{E},{1},{2}}, {{E},{1},{1,2}},{{E},{2},{1,2}},{{1},{2},{1,2}},
                                                {{E},{1},{2},{1,2}}} 
     |P(P({1,2}))| = 16.
 

  b. $\mathbb{Z}$ is the set of all integers. Powerset of $\mathbb{R}$ is the set of all possible subsets of $\mathbb{R}$. Since $\mathbb{Z}$ is a subset of $\mathbb{R}$, therefore $\mathbb{Z}$ is an element of the powerset of $\mathbb{R}$. Therefore $\mathbb{Z}$ $\in$ $\mathbb{R}$ but $\mathbb{Z}$ $\not \subset$ $\mathbb{R}$.

  c. 
*)

(*Problem 5*)
      datatype natural = Zero | Successor of natural;
      fun isEven(Zero) = true | isEven(Successor(n)) = not(isEven(n));

        fun times(Zero, m) = Zero |
        times(Successor(Zero),m) = m |
      times(Successor(n),m) = plus(m,times(n,m));

    fun plus(Zero,m) = m | plus(m, Zero) = m |
    plus(Successor(m), Successor(n)) = Successor(Successor(plus(m,n)));
     
(*Revision*)
  fun flatten(nil) = [] | flatten(x::xs) = x @ flatten(xs);
