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


(*Problem 5*)
      datatype natural = Zero | Successor of natural;
      fun isEven(Zero) = true | isEven(Successor(n)) = not(isEven(n));

        fun times(Zero, Zero) = Zero | times(Zero, Successor(n)) = Zero | times(Successor(n),Zero) = Zero |
        times(Successor(m),Successor(n)                                                            
(*Revision*)
  fun flatten(nil) = [] | flatten(x::xs) = x @ flatten(xs);
