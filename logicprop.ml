(*At Mentor Session *)
fun isElem(_,nil) = false
  | isElem(x,y::ys) = if x = y then true else isElem(x,ys);

datatype propForm = Var of string | Neg of propForm | And of propForm * propForm
                    |Or of propForm * propForm;
 
(*Taking in a propForm, and a list of strings with truth values*)
fun truthValue(Var(n),xs) = isElem(n,xs)
    |truthValue(Neg(p),xs)= not(truthValue(p,xs))
    |truthValue(And(p,q),xs) = truthValue(p,xs) andalso truthValue(q,xs)
    |truthValue(Or(p,q),xs) = truthValue(p,xs) orelse truthValue(q,xs);

 fun powerset(nil) = nil |
      powerset(x::xs) = map(fn x => x::ys, powerset(xs))@ powerset(xs);
