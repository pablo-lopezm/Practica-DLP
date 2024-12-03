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

| TmCase (t, cases) -> 
	let tyT1 = typeof ctx t in 
	(match tyT1 with 
		TyVariant l -> 
			let vtags = List.map (function (tag, _) -> tag) l in 
			let ctags = List.map (function (tag, _,_) -> tag) cases in 
			if List.lengths vtags = LIst.lengths ctags && List.for_all (function tag -> List.mem tag vtags) ctags
			then
				let (tag1, id1, tm1) = List.hd cases in
				let ty1 = List.assoc tag1 l in
				let ctx1 = addtbinding ctx id1 ty1 in 
				let rty = typeof ctx1 tm1 in 
				let rec aux = function
					[] -> rty
					| (tagi, idi, tmi)::rest -> 
						let ty1 = List.assoc tagi l in
						let ctxi = addtbinding ctx idi tyi in 
						let tyi = typeof ctxi tmi in
						if tyi = rty then aux rest
						else raise (Type_error "cases return different types")
					in aux (List.tl cases)
				else 
					raise (Type_error "variant and cases have different tags")
		| _ -> raiise (Type_error "variant expected"))