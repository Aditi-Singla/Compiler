val list_arg = CommandLine.arguments();


datatype  tree = NULL | Node of string * tree list;
exception Failure of string;
(*Get the root of the tree*)
fun getroot(NULL) = ""
|	getroot(Node(a,l)) = a;

(*Get the children of node a*)
fun getList(NULL) = []
|	getList(Node(a,l)) = l;	

(*
fun findNodeBool str:string t:tree = if t = NULL then false
								else if getroot(t) = str then true
								else List.foldl orcond false (map (findNodeBool str) tL);

fun findNode str t = if (findNodeBool str t) then 
						if getroot(t) = str then t
						else *)					


fun readFromFiletoCharList  (filename:string) = 
    let
    	val f = TextIO.getInstream(TextIO.openIn filename)
		fun loopNoNewline (clist, f) = 
		    case TextIO.StreamIO.input1 f of
			SOME (#"\n", f') => loopNoNewline (clist, f')
		      | SOME (c, f') => loopNoNewline (c::clist, f')
		      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in
    	rev(loopNoNewline ([], f))
    end;

fun remTab(s,l) = if s="" then l
				else if size(s)=1 then
					if s="\t" then l
					else l^s 
				else
					if (String.substring(s,0,1) = "\t") then remTab(String.substring(s,1,size(s)-1),l^" ")
					else remTab(String.substring(s,1,size(s)-1),l^String.substring(s,0,1));

fun readFromFiletoLineList (filename:string) =
    let
    	val f = TextIO.getInstream(TextIO.openIn filename)
		fun loop (accum: string list, f) =
		    case (TextIO.StreamIO.inputLine f) of 
		        SOME(chunk, f') => loop (remTab(String.substring(chunk,0,size(chunk)-1),"")::accum, f')
		      | NONE => (TextIO.StreamIO.closeIn f; accum)
	            (* esac *)
    in 
    	rev(loop ([], f))
    end;  

fun strToTuple (s) = let
						fun strToList (s:string,acc,l) = (if (s="") then l
								else if (String.substring(s,0,1)=" ") then strToList(String.substring(s,1,size(s)-1),"",l@acc::[])
								else if (size(s)=1) then strToList("","",l@(acc^s)::[])
								else strToList(String.substring(s,1,size(s)-1),acc^String.substring(s,0,1),l))

						val l = strToList(s,"",[])

					in
						if (length(l)=2) then 
							(2,(hd l,hd (tl l),"",""))
						else if (length(l)=4) then 
							(4,(hd l,hd (tl l),hd (tl (tl l)),hd (tl (tl (tl l)))))
						else 
							(0,("","","",""))
					end;

fun toTupleList([],acc) = acc
|	toTupleList(x::xs,acc) = toTupleList(xs,acc@strToTuple(x)::[]);

fun toTupleListofReservedWords([],l) = l
|	toTupleListofReservedWords(x::xs,l) = let
									val (a,(b,c,d,e)) = x
								in
									if (a = 2) then toTupleListofReservedWords(xs,l@x::[])
									else toTupleListofReservedWords(xs,l)
								end;

fun toTupleListofVar([],l) = l
|	toTupleListofVar(x::xs,l) = let
									val (a,(b,c,d,e)) = x
								in
									if (a = 4) then toTupleListofVar(xs,l@x::[])
									else toTupleListofVar(xs,l)
								end;

(*readFromFiletoLineList("input_sym.txt");
toTupleList(readFromFiletoLineList("input_sym.txt"),[]);
toTupleListofVar(toTupleList(readFromFiletoLineList("input_sym.txt"),[]),[]);			
*)
fun printStringtoFile (str:string, file:string) = 
    let 
      	val f =  TextIO.openAppend file
    in
    	(TextIO.output (f, str); TextIO.closeOut f) 
    end	;

(*Print the tree to file*)
fun printTree (tree,s,outputfile) =	case tree of
		NULL 	   => printStringtoFile("",outputfile)
	|	Node(a,tL) => (printStringtoFile((s^a),outputfile);
						if (tL =[]) then
							printStringtoFile("",outputfile)
						else(
							printStringtoFile("[\n",outputfile);	
							(let
								fun printS ([]) = printStringtoFile("",outputfile)
								|	printS ([x]) = printTree(x,s^"\t",outputfile)
								|	printS (x::xs) = (printTree(x,s^"\t",outputfile);printStringtoFile(",\n",outputfile);printS(xs));
							in
								printS(tL)		
							end);
							printStringtoFile("\n"^s^"]",outputfile)
						)	
					);						

fun isMember (s,[]) = false
	| isMember (s,(x::xs)) = if (s=x) then true else isMember(s,xs);

fun toTokenList ([],accum,s) = accum
	| toTokenList (l,accum,s) = if (isMember((hd l),[#"[",#"]",#","])) then 
									if s = "" then toTokenList(tl l,accum@[Char.toString(hd l)],"") else toTokenList(tl l,accum@[s]@[Char.toString(hd l)],"")
								else if (isMember(hd l,[#" ",#"\t"])) then
									if s = "" then toTokenList(tl l,accum,"") else toTokenList(tl l,accum@[s],"")
								else toTokenList(tl l,accum,s^(Char.toString(hd l)));		

(*Remove the last element of list*)
fun last l = rev(tl (rev l));	

(*Convert the string list to parsetree*)
fun stringtoASTTree ([]) = NULL
|	stringtoASTTree ([x]) = Node(x,[])
|	stringtoASTTree (l) = let
								fun listtoSubtrees ([],subtreelist:string list list,acc:string list,count) =  subtreelist
								|	listtoSubtrees (x::xs,subtreelist:string list list,acc:string list,count) = if (x="[") then listtoSubtrees(xs,subtreelist,acc@[x],count+1) 
																												else if (x="]") then
																													if (count-1=0) then listtoSubtrees(xs,subtreelist@[acc@[x]],[],count-1)
																													else listtoSubtrees(xs,subtreelist,acc@[x],count-1)
																												else if (x= ",") then 
																													if (count=0) then listtoSubtrees(xs,subtreelist,acc,count)
																													else listtoSubtrees(xs,subtreelist,acc@[x],count)
																												else 
																													if (count = 0) then
																														if (xs =[] orelse (hd xs = ",")) then listtoSubtrees(xs,subtreelist@[acc@[x]],[],count)
																														else listtoSubtrees(xs,subtreelist,acc@[x],count)
																													else listtoSubtrees(xs,subtreelist,acc@[x],count)
							in
								Node(hd l,map (stringtoASTTree) (listtoSubtrees(tl (tl (last l)),[],[],0)))
							end;
fun printASTtofile (input,output) = (printTree(stringtoASTTree(toTokenList(readFromFiletoCharList(input),[],"")),"",output);printStringtoFile("\n",output));

fun isPosInt(s) = if s="" then true
				else if size(s)= 1 then isMember(s,["0","1","2","3","4","5","6","7","8","9"])
				else isMember(String.substring(s,0,1),["0","1","2","3","4","5","6","7","8","9"]) andalso isPosInt(String.substring(s,1,size(s)-1));

fun getscopesymbols ([],scope,l) = l
|	getscopesymbols (x::xs,scope,l) = let
										val (a,(b,c,d,e)) = x
									in
										if (e = scope) then getscopesymbols(xs,scope,l@x::[])
										else getscopesymbols(xs,scope,l)
									end;

fun toLowerCase(s) = if s = "" then ""
					else if size(s)=1 then Char.toString(Char.toLower(String.sub(s,0)))
					else Char.toString(Char.toLower(String.sub(s,0)))^toLowerCase(String.substring(s,1,size(s)-1));

fun checkProcSeq([],symtable,scope,scopesymbols) = true
|	checkProcSeq(x::xs,symtable,scope,scopesymbols) = case x of
				(Node("proc",[Node("ProcName",[Node(a,[])]),b])) => typecheck(b,symtable,scope^":"^a,scopesymbols@getscopesymbols(symtable,scope^":"^a,[])) andalso checkProcSeq(xs,symtable,scope,scopesymbols)
			|	_ => raise Failure("Syntax Error in procedure declaration at "^scope^" level!")	
												
and getReservedWords([],l) = l
|	getReservedWords(x::xs,l) = let
									val (a,(b,c,d,e)) = x
								in
									getReservedWords(xs,l@[b])
								end

and checkscope([],symtable) = true
|	checkscope(x::xs,symtable) = let
							val (a,(b,c,d,e)) = x
						in
							if not (isMember(toLowerCase(b),getReservedWords(symtable,[]))) then
								if d = "INT" orelse d="BOOL" then
									if isMember((a,(b,c,"INT",d)),xs) orelse isMember((a,(b,c,"BOOL",d)),xs) then raise Failure("Error: Variable '"^b^"' declared twice at same level of scope: "^e^"!")
									else checkscope(xs,symtable)
								else true
							else raise Failure("Variable cannot be assigned the name of reserved word:"^b)			
						end	

and findType(a,[]) = raise Failure("Variable "^a^" not defined in the given scope!")		
|	findType(a,x::xs) = let
							val (p,(q,r,s,t)) = x
						in
							if q=a then s
							else findType(a,xs)	
						end				

and checkBoolexp(t,scopesymbols,scope) = case t of
				NULL => false
			|	Node(a,[]) => a="tt" orelse a = "ff" orelse findType(a,scopesymbols) = "BOOL"	
			|	Node("BINADD",l) => false
			|	Node("BINSUB",l) => false
			|	Node("BINMUL",l) => false
			|	Node("BINDIV",l) => false
			|	Node("BINMOD",l) => false
			|	Node("GTE",[a,b]) => (checkBoolexp(a,scopesymbols,scope) andalso checkBoolexp(b,scopesymbols,scope)) orelse (checkIntexp(a,scopesymbols,scope) andalso checkIntexp(b,scopesymbols,scope))
			|	Node("GT",[a,b]) => (checkBoolexp(a,scopesymbols,scope) andalso checkBoolexp(b,scopesymbols,scope)) orelse (checkIntexp(a,scopesymbols,scope) andalso checkIntexp(b,scopesymbols,scope))
			|	Node("LTE",[a,b]) => (checkBoolexp(a,scopesymbols,scope) andalso checkBoolexp(b,scopesymbols,scope)) orelse (checkIntexp(a,scopesymbols,scope) andalso checkIntexp(b,scopesymbols,scope))
			|	Node("LT",[a,b]) => (checkBoolexp(a,scopesymbols,scope) andalso checkBoolexp(b,scopesymbols,scope)) orelse (checkIntexp(a,scopesymbols,scope) andalso checkIntexp(b,scopesymbols,scope))
			|	Node("NE",[a,b]) => (checkBoolexp(a,scopesymbols,scope) andalso checkBoolexp(b,scopesymbols,scope)) orelse (checkIntexp(a,scopesymbols,scope) andalso checkIntexp(b,scopesymbols,scope))
			|	Node("EQ",[a,b]) => (checkBoolexp(a,scopesymbols,scope) andalso checkBoolexp(b,scopesymbols,scope)) orelse (checkIntexp(a,scopesymbols,scope) andalso checkIntexp(b,scopesymbols,scope))
			|	Node("AND",[a,b]) => (checkBoolexp(a,scopesymbols,scope) andalso checkBoolexp(b,scopesymbols,scope))
			|	Node("OR",[a,b]) => (checkBoolexp(a,scopesymbols,scope) andalso checkBoolexp(b,scopesymbols,scope))
			|	Node("UMINUS",[a]) => false
			|	Node("NEG",[a]) => (checkBoolexp(a,scopesymbols,scope))
			|	_ => false	

and checkIntexp(t,scopesymbols,scope) = case t of
				NULL => false
			|	Node(a,[]) => isPosInt(a) orelse findType(a,scopesymbols) = "INT"
			|	Node("BINADD",[a,b]) => (checkIntexp(a,scopesymbols,scope) andalso checkIntexp(b,scopesymbols,scope))
			|	Node("BINSUB",[a,b]) => (checkIntexp(a,scopesymbols,scope) andalso checkIntexp(b,scopesymbols,scope))
			|	Node("BINMUL",[a,b]) => (checkIntexp(a,scopesymbols,scope) andalso checkIntexp(b,scopesymbols,scope))
			|	Node("BINDIV",[a,b]) => (checkIntexp(a,scopesymbols,scope) andalso checkIntexp(b,scopesymbols,scope))
			|	Node("BINMOD",[a,b]) => (checkIntexp(a,scopesymbols,scope) andalso checkIntexp(b,scopesymbols,scope))
			|	Node("GTE",[a,b]) => false
			|	Node("GT",[a,b]) => false
			|	Node("LTE",[a,b]) => false
			|	Node("LT",[a,b]) => false
			|	Node("NE",[a,b]) => false
			|	Node("EQ",[a,b]) => false
			|	Node("AND",[a,b]) => false
			|	Node("OR",[a,b]) => false
			|	Node("UNMINUS",[a]) => checkIntexp(a,scopesymbols,scope)
			|	Node("NEG",[a]) => false
			|	_ => false

and checkCmdSeq([],scopesymbols,scope) = true
|	checkCmdSeq(x::xs,scopesymbols,scope) = case x of
					(Node("print",[Node(a,[])])) => if findType(a,scopesymbols) = "INT" orelse findType(a,scopesymbols) = "BOOL" then checkCmdSeq(xs,scopesymbols,scope)
										else raise Failure("Variable "^a^"not of type INT or BOOL")
				|	(Node("read",[Node(a,[])])) => if findType(a,scopesymbols) = "INT" orelse findType(a,scopesymbols) = "BOOL" then checkCmdSeq(xs,scopesymbols,scope)
										else raise Failure("Variable "^a^"not of type INT or BOOL")
				|	(Node("call",[Node(a,[])])) => if findType(a,scopesymbols) = "PROC" then checkCmdSeq(xs,scopesymbols,scope)
										else raise Failure("Variable "^a^"not of type PROC")
				|	(Node("ASSIGN",[Node(a,[]),b])) => if findType(a,scopesymbols)="BOOL" then
													(if (checkBoolexp(b,scopesymbols,scope)) then checkCmdSeq(xs,scopesymbols,scope)
													else raise Failure(a^" is of type Bool while the value assigned to it is not a BoolExpression!"))
											else if findType(a,scopesymbols)="INT" then
													(if (checkIntexp(b,scopesymbols,scope)) then checkCmdSeq(xs,scopesymbols,scope)
													else raise Failure(a^" is of type Int while the value assigned to it is not a IntExpression!"^scope))
											else raise Failure("Invalid type for assignment command")
				|	(Node("EPSILON",[])) => true
				|	(Node("while",[a,Node("CommandSeq",b)])) => 
											if (checkBoolexp(a,scopesymbols,scope)) then
												if checkCmdSeq(b,scopesymbols,scope) then checkCmdSeq(xs,scopesymbols,scope)
												else raise Failure("Invalid type for while command : do ..")
											else raise Failure("Invalid type for while command : while ..")
				|	(Node("Conditional",[(Node("if",[a])),(Node("then",[Node("CommandSeq",b)])),(Node("else",[Node("CommandSeq",c)]))]))
										=>	if checkBoolexp(a,scopesymbols,scope) then
												if checkCmdSeq(b,scopesymbols,scope) then 
													if checkCmdSeq(c,scopesymbols,scope) then checkCmdSeq(xs,scopesymbols,scope)
													else raise Failure("Invalid type for if-then-else Conditional command : else ..")
												else raise Failure("Invalid type for if-then-else Conditional command : then ..")
											else raise Failure("Invalid type for if-then-else Conditional command : if ..")
				|	_		=>	raise Failure("Syntax Error: Unknown Command in CommandSeq at "^scope^" level!")												
													
and typecheck (t,symtable,scope,scopesymbols) = 
		if checkscope(scopesymbols,toTupleListofReservedWords(symtable,[])) then
				 (case t of
					NULL => true
				|	Node("Block",[Node("DeclarationSeq",[a,b,Node("PROCS",l1)]),Node("CommandSeq",l2)]) => (case (l1,l2) of
																									([Node("EPSILON",[])],[Node("EPSILON",[])]) => true
																								|	([Node("EPSILON",[])],l) => checkCmdSeq(l,scopesymbols,scope)
																								|	(l,[Node("EPSILON",[])]) => checkProcSeq(l,symtable,scope,scopesymbols)
																								|	(l1,l2) => 	checkProcSeq(l1,symtable,scope,scopesymbols) andalso checkCmdSeq(l2,scopesymbols,scope))
				|	_ => raise Failure ("Syntax Error")																				
					)
		else
			false;

(*getReservedWords(toTupleListofReservedWords(toTupleList(readFromFiletoLineList(hd (tl (tl list_arg))),[]),[]),[]);
printASTtofile(hd list_arg,hd (tl list_arg));
typecheck(stringtoASTTree(toTokenList(readFromFiletoCharList(hd list_arg),[],"")),toTupleList(readFromFiletoLineList(hd (tl (tl list_arg))),[]),"global",getscopesymbols(toTupleList(readFromFiletoLineList(hd (tl (tl list_arg))),[]),"global",[]));
*)
fun getDeclCmds (stype,[],acc) = acc
|	getDeclCmds (stype,x::xs,acc) = case x of
					Node("EPSILON",[]) => acc
				|	Node(a,[]) => if stype = "int" then getDeclCmds(stype,xs,acc@["DECLARE_INT "^a^" _ _"])
								else getDeclCmds(stype,xs,acc@["DECLARE_BOOL "^a^" _ _"])
				|	_	=> acc					

and getProcNames ([],acc) = acc
|	getProcNames	(x::xs,acc) = case x of
					Node("EPSILON",[]) => acc
				|	Node("proc",[Node("ProcName",[Node(a,[])]),b]) => getProcNames(xs,acc@[a])
				|	_	=> acc

and getProcCmds ([],acc,position,symtable,scope,scopesymbols,p) = (acc,p)
|	getProcCmds	(x::xs,acc,position,symtable,scope,scopesymbols,p) = case x of
					Node("EPSILON",[]) => (acc,p)
				|	Node("proc",[Node("ProcName",[Node(a,[])]),b]) => let
																		val t = codeGenerator(b,symtable,scope^":"^a,scopesymbols@getscopesymbols(symtable,scope^":"^a,[]),[],position)
																	in
																		getProcCmds(xs,acc@t,position+(length(t)),symtable,scope,scopesymbols,p@[position+length(t)])
																	end
				|	_ => (acc,p)

and getProcDecl ([],acc,positionsofprocs) = acc
|	getProcDecl (x::xs,acc,[]) = acc	
|	getProcDecl	(x::xs,acc,y::ys) = case x of
					Node("EPSILON",[]) => acc
				|	Node("proc",[Node("ProcName",[Node(a,[])]),b]) => getProcDecl(xs,acc@["DECLARE_PROC "^a^" "^Int.toString(y)^" _"],ys)
				|	_ => acc

and simplify(p,str,n,l,scope,scopesymbols) = case p of 
					Node(a,[]) => if a = "tt" orelse a = "ff" then (str^"."^Int.toString(n),l@["DECLARE_BOOL "^str^"."^Int.toString(n)^" _ _","ASSIGN "^a^" _ "^str^"."^Int.toString(n)])
								else if isPosInt(a) then (str^"."^Int.toString(n),l@["DECLARE_INT "^str^"."^Int.toString(n)^" _ _","ASSIGN "^a^" _ "^str^"."^Int.toString(n)])
								else (a,l)
				|	Node("BINADD",[a,b]) => let
												val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
												val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
											in	
												(str^"."^Int.toString(n),l@r@t@["DECLARE_INT "^str^"."^Int.toString(n)^" _ _","PLUS "^q^" "^s^" "^str^"."^Int.toString(n)])
											end	
				|	Node("BINSUB",[a,b]) => let
												val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
												val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
											in	
												(str^"."^Int.toString(n),l@r@t@["DECLARE_INT "^str^"."^Int.toString(n)^" _ _","MINUS "^q^" "^s^" "^str^"."^Int.toString(n)])
											end
				|	Node("BINMUL",[a,b]) => let
												val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
												val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
											in	
												(str^"."^Int.toString(n),l@r@t@["DECLARE_INT "^str^"."^Int.toString(n)^" _ _","MULT "^q^" "^s^" "^str^"."^Int.toString(n)])
											end
				|	Node("BINDIV",[a,b]) => let
												val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
												val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
											in	
												(str^"."^Int.toString(n),l@r@t@["DECLARE_INT "^str^"."^Int.toString(n)^" _ _","DIV "^q^" "^s^" "^str^"."^Int.toString(n)])
											end
				|	Node("BINMOD",[a,b]) => let
												val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
												val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
											in	
												(str^"."^Int.toString(n),l@r@t@["DECLARE_INT "^str^"."^Int.toString(n)^" _ _","MOD "^q^" "^s^" "^str^"."^Int.toString(n)])
											end
				|	Node("GTE",[a,b]) => let
											val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
											val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
										in	
											(str^"."^Int.toString(n),l@r@t@["DECLARE_BOOL "^str^"."^Int.toString(n)^" _ _","GTE "^q^" "^s^" "^str^"."^Int.toString(n)])
										end
				|	Node("GT",[a,b]) => let
											val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
											val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
										in	
											(str^"."^Int.toString(n),l@r@t@["DECLARE_BOOL "^str^"."^Int.toString(n)^" _ _","GT "^q^" "^s^" "^str^"."^Int.toString(n)])
										end
				|	Node("LTE",[a,b]) => let
											val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
											val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
										in	
											(str^"."^Int.toString(n),l@r@t@["DECLARE_BOOL "^str^"."^Int.toString(n)^" _ _","LTE "^q^" "^s^" "^str^"."^Int.toString(n)])
										end
				|	Node("LT",[a,b]) => let
											val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
											val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
										in	
											(str^"."^Int.toString(n),l@r@t@["DECLARE_BOOL "^str^"."^Int.toString(n)^" _ _","LT "^q^" "^s^" "^str^"."^Int.toString(n)])
										end
				|	Node("NE",[a,b]) => let
											val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
											val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
										in	
											(str^"."^Int.toString(n),l@r@t@["DECLARE_BOOL "^str^"."^Int.toString(n)^" _ _","NEQ "^q^" "^s^" "^str^"."^Int.toString(n)])
										end
				|	Node("EQ",[a,b]) => let
											val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
											val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
										in	
											(str^"."^Int.toString(n),l@r@t@["DECLARE_BOOL "^str^"."^Int.toString(n)^" _ _","EQ "^q^" "^s^" "^str^"."^Int.toString(n)])
										end
				|	Node("AND",[a,b]) => let
											val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
											val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
										in	
											(str^"."^Int.toString(n),l@r@t@["DECLARE_BOOL "^str^"."^Int.toString(n)^" _ _","AND "^q^" "^s^" "^str^"."^Int.toString(n)])
										end
				|	Node("OR",[a,b]) => let
											val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
											val (s,t) = simplify(b,str^"."^Int.toString(n),1,[],scope,scopesymbols)
										in	
											(str^"."^Int.toString(n),l@r@t@["DECLARE_BOOL "^str^"."^Int.toString(n)^" _ _","OR "^q^" "^s^" "^str^"."^Int.toString(n)])
										end
				|	Node("UNMINUS",[a]) => let
											val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
										in	
											(str^"."^Int.toString(n),l@r@["DECLARE_INT "^str^"."^Int.toString(n)^" _ _","UNMINUS "^q^" _ "^str^"."^Int.toString(n)])
										end
				|	Node("NEG",[a]) => let
											val (q,r) = simplify(a,str^"."^Int.toString(n),0,[],scope,scopesymbols)
										in	
											(str^"."^Int.toString(n),l@r@["DECLARE_BOOL "^str^"."^Int.toString(n)^" _ _","NOT "^q^" _ "^str^"."^Int.toString(n)])
										end
				|	_ => ("",l)


and getCmds([],s:string list,scope,i,scopesymbols,position) = s
|	getCmds(x::xs,s:string list,scope,i,scopesymbols,position) = case x of
					Node("EPSILON",[]) => s
				|	Node("read",[Node(a,[])]) => getCmds(xs,s@[("READ _ _ "^a)],scope,i+1,scopesymbols,position)
				|	Node("print",[Node(a,[])]) => getCmds(xs,s@["PRINT "^a^" _ _"],scope,i+1,scopesymbols,position)
				|	Node("call",[Node(a,[])]) => getCmds(xs,s@["CALL "^a^" _ _"],scope,i+1,scopesymbols,position)
				|	Node("ASSIGN",[Node(c,l1),d]) => (case d of

									Node(a,[]) => (*if a = "tt" orelse a = "ff" then getCmds(xs,s@["DECLARE_BOOL "^"."^scope^"."^Int.toString(i)^" _ _","ASSIGN "^a^" _ "^"."^scope^"."^Int.toString(i),"ASSIGN "^"."^scope^"."^Int.toString(i)^" _ "^c],scope,i+1,scopesymbols,position)
												else if isPosInt(a) then getCmds(xs,s@["DECLARE_INT "^"."^scope^"."^Int.toString(i)^" _ _","ASSIGN "^a^" _ "^"."^scope^"."^Int.toString(i),"ASSIGN "^"."^scope^"."^Int.toString(i)^" _ "^c],scope,i+1,scopesymbols,position)
												else *)getCmds(xs,s@["ASSIGN "^a^" _ "^c],scope,i+1,scopesymbols,position)

								|	Node("BINADD",[a,b]) => (let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["PLUS "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end)
								|	Node("BINSUB",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["MINUS "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("BINMUL",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["MULT "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("BINDIV",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["DIV "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("BINMOD",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["MOD "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("GTE",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["GTE "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("GT",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["GT "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("LTE",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["LTE "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("LT",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["LT "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("NE",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["NEQ "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("EQ",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["EQ "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("AND",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["AND "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("OR",[a,b]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val (u,t) = simplify(b,"."^scope^"."^Int.toString(i),1,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@t@["OR "^q^" "^u^" "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("UNMINUS",[a]) => let
																val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
															in	
																getCmds(xs,s@r@["UMINUS "^q^" _ "^c],scope,i+1,scopesymbols,position)
															end
								|	Node("NEG",[a]) => let
															val (q,r) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
														in	
															getCmds(xs,s@r@["NOT "^q^" _ "^c],scope,i+1,scopesymbols,position)
														end
								|	_			=> s
							)
		
				|	Node("Conditional",[Node("if",[a]),Node("then",[Node("CommandSeq",l1)]),Node("else",[Node("CommandSeq",l2)])]) 
											=>	let
													val (l,m) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
													val n2 = getCmds(l2,[],scope,i+2000,scopesymbols,position+length(s)+length(m)+1)
													val n1 = getCmds(l1,[],scope,i+1000,scopesymbols,position+length(s)+length(m)+1+length(n2)+1)
												in
													getCmds(xs,s@m@["IF "^l^" "^Int.toString(position+length(s)+length(m)+1+length(n1)+1)^" _"]@n1@["GOTO _ "^Int.toString(position+length(s)+length(m)+1+length(n2)+1+length(n1))^" _"]@n2,scope,i+1,scopesymbols,position)
												end 

				|	Node("while",[a,Node("CommandSeq",l1)])	=> let
																val (l,m) = simplify(a,"."^scope^"."^Int.toString(i),0,[],scope,scopesymbols)
																val n1 = getCmds(l1,[],scope,i+1000,scopesymbols,position+length(s)+length(m)+1)
															in
																getCmds(xs,s@m@["IF "^l^" "^Int.toString(position+length(s)+length(m)+1+length(n1)+1)^" _"]@n1@["GOTO _ "^Int.toString(position+length(s))^" _"],scope,i+1,scopesymbols,position)
															end						

and codeGenerator (t,symtable,scope,scopesymbols,s,position) = if typecheck(t,symtable,scope,scopesymbols) then
				(case t of
					NULL => s
				|	Node("Block",[Node("DeclarationSeq",[Node("int",l1),Node("bool",l2),Node("PROCS",l3)]),Node("CommandSeq",l4)]) 
							 => let
									val s_int = getDeclCmds ("int",l1,[])
									val s_bool = getDeclCmds ("bool",l2,[])
									val l_procs = getProcNames (l3,[])
									val s_cmd = getCmds (l4,[],scope,0,scopesymbols,position+length(s_int)+length(s_bool)+length(l_procs))
									val po = position+length(s_int)+length(s_bool)+length(l_procs)+length(s_cmd)+1
									val (s_procs,positionsofprocs) = getProcCmds (l3,[],po,symtable,scope,scopesymbols,[po])
									val s_procDecl = getProcDecl (l3,[],positionsofprocs)
								in
									if scope = "global" then
										s@s_int@s_bool@s_procDecl@s_cmd@["END_OF_CODE _ _ _"]@s_procs
									else
										s@s_int@s_bool@s_procDecl@s_cmd@["RETURN _ _ _"]@s_procs	
								end
				|	_ => raise Failure ("Syntax Error")																				
					)
				else raise Failure ("Type errors!");

fun printS ([],file) = printStringtoFile("",file)
|	printS (x::xs,file) = (printStringtoFile(x^"\n",file);printS(xs,file));

(*codeGenerator(stringtoASTTree(toTokenList(readFromFiletoCharList(hd list_arg),[],"")),toTupleList(readFromFiletoLineList(hd (tl list_arg)),[]),"global",getscopesymbols(toTupleList(readFromFiletoLineList(hd (tl list_arg)),[]),"global",[]),[],0);*)

printS(codeGenerator(stringtoASTTree(toTokenList(readFromFiletoCharList(hd list_arg),[],"")),toTupleList(readFromFiletoLineList(hd (tl list_arg)),[]),"global",getscopesymbols(toTupleList(readFromFiletoLineList(hd (tl list_arg)),[]),"global",[]),[],0),hd (tl (tl list_arg)));
val out_code = hd (tl (tl list_arg));
val out_value = hd (rev list_arg);
use "stack_machine.sml";

OS.Process.exit(OS.Process.success);