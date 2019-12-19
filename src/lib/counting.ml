(* Algorithms for counting the number of elements of combinatorial classes. *)

(* type specs : {names: string array; rules: int expression array} *)

let rec count arrays (specs: Grammar.t) (expr: int Grammar.expression) (n: int) (iName: int) (y: int) = (* counting arrays[iName][y] *)
   let tmp = (Array.get (Array.get arrays iName) y) in
   if tmp != -1 then tmp
   else
     begin
        match (expr) with
        | Z i ->(*print_string "case Z ATOME\n"; *)
		let res = (if(i == y) then 1 else 0) in
		     Array.set (Array.get arrays iName) y res;
		     res
        | Product(op1, op2) -> (*print_string "case PRODUCT\n";*)
			       let sum = ref 0 in
	    		       for k = 0 to y do
			          let op1_k = count arrays specs op1 n iName k in
				  let op2_n_k = count arrays specs op2 n iName (y-k) in
				  sum := (!sum + op1_k * op2_n_k)
			       done;
			       Array.set (Array.get arrays iName) y (!sum);
			       !sum

        |Union(op1, op2) -> (*print_string "case UNION\n";*)
			    let op1_n = count arrays specs op1 n iName y in
			    let op2_n = count arrays specs op2 n iName y in
			    let res = op1_n + op2_n in
			    Array.set (Array.get arrays iName) y res;
			    res

	| Reference r -> (*print_string "case REFERENCE ";*)
		count arrays specs (Array.get specs.rules r) n r y

	| Seq(_) -> -1 (* not handled yet *)
     end


let rec hasAtMostAtomeSizeZero (expr: int Grammar.expression) = 
	match expr with
	| Z 0 -> true
	| Z _ -> false
	| Product(op1,op2) -> (hasAtMostAtomeSizeZero op1) && (hasAtMostAtomeSizeZero op2)
	| _ -> true

let rec countUnionProductZero arrays (specs: Grammar.t) (expr: int Grammar.expression) (iName: int) =
   match expr with
   | Z n -> if(n == 0) then 1 else 0
   | Union(op1, op2) -> let countOp1 = countUnionProductZero arrays specs op1 iName in
			   let countOp2 = countUnionProductZero arrays specs op2 iName in
			      (countOp1 + countOp2)
   | Product(op1, op2) -> if(hasAtMostAtomeSizeZero expr) then
			  	(let countOp1 = countUnionProductZero arrays specs op1 iName in
			     	let countOp2 = countUnionProductZero arrays specs op2 iName in
			        	(countOp1 * countOp2)
                                )
			  else 0
   | Reference r -> if( (Array.get (Array.get arrays r) 0) != -1) then 
			(Array.get (Array.get arrays r) 0) else
			begin
			   countSizeZero arrays specs (Array.get specs.rules r) r;
			    if( (Array.get (Array.get arrays r) 0) != -1) then
				(Array.get (Array.get arrays r) 0)  else
				(print_string "should not happen countUnionZero\n"; 
				(-1) ) 
			end	
   | _ -> -1 (* not handled *)
			

and countSizeZero arrays (specs: Grammar.t) (expr: int Grammar.expression) (iName: int) =
   match expr with
   | Z n -> if(n == 0) then ( (Array.set (Array.get arrays iName) 0 1)) else (Array.set (Array.get arrays iName) 0 0)
   | Union(_, _) -> let count = (countUnionProductZero arrays specs expr iName) in
			   ((Array.set (Array.get arrays iName) 0 count))
   | Product(_,_) -> let count = (countUnionProductZero arrays specs expr iName) in
			    ((Array.set (Array.get arrays iName) 0 count)) 
   | _ -> () (* not handled *)

let countAll (specs: Grammar.t) n = 
   let specSize = (Array.length specs.names) in
   let (countArrays: int array array) = (Array.make_matrix specSize (n+1) (-1)) in
   (* countArrays[i][y] = count(specs.names[i], y), if = -1 not yet computed *)
   for j = 0 to (specSize-1) do 
      Printf.printf "names[%d] = %s\n" j (Array.get specs.names j);
   done;
   for j = 0 to (specSize-1) do 
      countSizeZero countArrays specs (Array.get specs.rules j) j
   done;
   for iName = 0 to (specSize-1) do
      if ((Array.get (Array.get countArrays iName) 0) == -1) then 
	(Array.set (Array.get countArrays iName) 0 0) else ()
   done;
   (for iName = 0 to (specSize-1) do
      (for y = 1 to n do
	let (expr: int Grammar.expression) = (Array.get specs.rules iName) in
      	let _ = (count countArrays specs (expr) n iName y) in ()
      done);
   done);
   countArrays


