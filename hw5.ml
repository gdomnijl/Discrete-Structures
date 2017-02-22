(*homework 5*)
(*Problem 1*)
fun totalNumVariables(Var(s))= 1
|totalNumVariables(And(p,q)) = totalNumVariables(p) + totalNumVariables(q)
|totalNumVariables(Or(p,q)) = totalNumVariables(p) + totalNumVariables(q)
|totalNumVariables(Neg(p)) =  totalNumVariables(p);
(*Problem 3a*)
  fun isSatisfiable(p) = isElem(true,allTruthValues(p));
(*Problem 3b*)
    fun isTautology(p) = not(isElem(false,allTruthValues(p)));
      (*citation: reminded by Maddie for the tricky part*)

(*Problem 4a*)

 fun logicallyEquiv(p,q) = let val assignment1 = powerSet(occurringVars(q))
                               val assignment2 = powerSet(occurringVars(p)) in
  if occurringVars(p) = occurringVars(q)
 then allTruthValues(p)=allTruthValues(q)
 else if length(occurringVars(p)) > length(occurringVars(q))
 then 
 allTruthValues(q) = listMap(fn n=>truthValue(p,n), assignment1) 
else
allTruthValues(p) = listMap(fn n=>truthValue(q,n), assignment2) end;


(*Problem 4b*)


(*Problem 5a*)
fun includeNeg(Var(s)) = Var(s)
|includeNeg(Neg(Var(s))) = Neg(Var(s))
|includeNeg(Neg(And(p,q))) = Or(Neg(includeNeg(p)),(Neg(includeNeg(q))))
|includeNeg(Neg(Or(p,q))) = And(Neg(includeNeg(p)),(Neg(includeNeg(q))))
|includeNeg(Neg(Neg(p))) = includeNeg(p);

  
(*Problem 5b*)
