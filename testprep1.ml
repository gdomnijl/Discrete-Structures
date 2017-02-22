fun longer(nil,nil) = 0
| longer(nil, ys) = 1
| longer(xs, nil) = ~1
| longer(x::xs,y::ys) = longer(xs,ys);

  fun reverse("")="" |
      reverse(s)= reverse(substring(s,1,size(s)-1))^substring(s,0,1);
fun cartprod(xs,[])=[]
   |cartprod(xs,y::ys) =
       let val pairs = map(fn x=>(x,y),xs)
        in pairs::cartprod(xs,ys)
        end;
(*do no recursion but two maps*)

(*To create a type class that for each 'class' we can access its fields:
name,number,prereq
###Note: we always need a basecase if the type itself is recursive*)
datatype class = NONE | NAME of string * int * class

  fun prereq(NONE) = nil |
      prereq(NAME(x,y,[])= nil|
      prereq(NAME(x,y,z::p::xs)) = p; 
