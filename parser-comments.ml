add = letrec add : <pos:Nat, zero:Bool, neg:Nat> -> <pos:Nat, zero:Bool, neg:Nat> -> <pos:Nat, zero:Bool, neg:Nat> = 
	lambda i1: <pos:Nat, zero:Bool, neg:Nat>. lambda i2: <pos:Nat, zero:Bool, neg:Nat>.
	case i1 of 
	<zero=z1> => i2
	| <pos=p1> => 
	(case i2 of 
		<zero=z2> => i1
		| <pos=p2> => (<pos=sum p1 p2> as <pos:Nat, zero:Bool, neg:Nat>)
		| <neg=n2> =>
		  (if iszero p1 then 
		  	if iszero n2 then 
		  	  <zero=true> as <pos:Nat, zero:Bool, neg:Nat>
		  	else 
		  	  <neg=n2> as <pos:Nat, zero:Bool, neg:Nat>
		   else 
		     if iszero n2 then 
		     	<pos=p1> as <pos:Nat, zero:Bool, neg:Nat>
		    else 
		      add (<pos=pred p1> as <pos:Nat, zero:Bool, neg:Nat>) (<neg=pred n2> as <pos:Nat, zero:Bool, neg:Nat>)))
		      
       | <neg=n1> => 
       	  (case i2 of 
       	     <zero=z2> => i1
       	     | <pos=p2> => add i2 i1
       	     | <neg=n2> => (<neg=sum n1 n2> as <pos:Nat, zero:Bool, neg:Nat>))
 in add;;

sum = 
  letrec sum : Nat -> Nat -> Nat = 
    lambda n : Nat. lambda m : Nat. if iszero n then m else succ (sum (pred n) m)
in sum;;