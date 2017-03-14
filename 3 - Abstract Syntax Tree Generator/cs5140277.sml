val list_arg = CommandLine.arguments();
datatype  tree = NULL | Node of string * tree list;

(*Get the root of the tree*)
fun getroot(NULL) = ""
|	getroot(Node(a,l)) = a;

(*Get the children of node a*)
fun getList(NULL) = []
|	getList(Node(a,l)) = l;	

fun readFromFile  (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loopNoNewline (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (#"\n", f') => loopNoNewline (clist, f')
	      | SOME (c, f') => loopNoNewline (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loopNoNewline ([], f))
    end;

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
fun stringtoParseTree ([]) = NULL
|	stringtoParseTree ([x]) = Node(x,[])
|	stringtoParseTree (l) = let
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
								Node(hd l,map (stringtoParseTree) (listtoSubtrees(tl (tl (last l)),[],[],0)))
							end;

(*Obtain all the variables declared into a list as Node(Variable,[])*)
fun varDef (NULL,acc) = acc
|	varDef (Node(a,l),acc) = if ((hd l)=Node("EOS",[])) then acc
								else if (hd l=Node("COMMA",[])) then varDef(hd (tl l),acc)
								else varDef(hd (tl l),acc@[toAST(hd l)])

(*Obtain the list of all processes declared into a list with each proc as Node(Proc,[ProcName,Block])*)
and proc (l,acc) = if (hd l=Node("EPSILON",[])) then acc
				else proc( getList(hd (rev l)) , acc @ ([Node("proc",[Node("ProcName",[toAST(hd (tl l))]),(toAST (hd (tl (tl l))))])]) )

(*Obtains the list of all the commands with each command as a node under the CommandSeq node*)
and cmd (NULL,acc) = []
|	cmd (Node(a,tL),acc) = if (tL = [Node("EPSILON",[])]) then acc
							else cmd (hd (rev tL),acc@[toAST(hd tL)])	

and toAST (NULL) = NULL
|	toAST (Node(root,treeList)) = case root of

					"Program" => toAST(hd treeList)

				|	"Block"	  => Node("Block",map toAST treeList)

				|	"DeclarationSeq" => (case treeList of
							[Node(a,l1),Node(b,l2)] => if (hd l2=Node("EPSILON",[])) then Node("DeclarationSeq",(map toAST l1)@[Node("PROCS",[Node("EPSILON",[])])])
														else Node("DeclarationSeq",(map toAST l1)@[Node("PROCS",proc(l2,[]))])
							| _ => Node("DeclarationSeq",[]))
				
				|	"CommandSeq" => if (getList(hd (tl treeList))=[Node("EPSILON",[])]) then Node("CommandSeq",[Node("EPSILON",[])])
																else Node("CommandSeq",cmd(hd (tl treeList),[]))
				
				|	"IntVarDecls" => if (hd treeList=Node("EPSILON",[])) then Node("int",[Node("EPSILON",[])])
										else Node("int",varDef((hd (tl treeList)),[]))
				
				|	"BoolVarDecls" => if (hd treeList=Node("EPSILON",[])) then Node("bool",[Node("EPSILON",[])])
										else Node("bool",varDef((hd (tl treeList)),[]))
				
				|	"AssignmentCmd" => Node("ASSIGN",[toAST(hd treeList),toAST(hd (rev treeList))])  (*change expreession to toAST later*)
				
				|	"CallCmd" => Node("call",[toAST(hd (tl treeList))])
				
				|	"ReadCmd" => Node("read",[toAST(hd (tl (tl treeList)))])
				
				|	"PrintCmd" =>  Node("print",[toAST(hd (tl (tl treeList)))])
				
				|	"ConditionalCmd" => Node("Conditional",[(Node("if",[toAST(hd (tl treeList))])),(Node("then",[toAST(hd (tl (tl (tl treeList))))])),(Node("else",[toAST(hd (rev treeList))]))])
				
				|	"WhileCmd" => Node("while",[toAST(hd (tl treeList)),toAST(hd (tl (tl treeList)))])
				
				|	"Expression" => toAST(hd treeList)
				
				|	"BoolExpression" => if (getList(hd (tl treeList))=[Node("EPSILON",[])]) then toAST(hd treeList)
										else Node((getroot(hd(getList(hd (tl treeList))))),[toAST(hd treeList),toAST(hd(tl (getList(hd (tl treeList)))))])
				
				|	"BoolF" => if (getList(hd (tl treeList))=[Node("EPSILON",[])]) then toAST(hd treeList)
								else Node(getroot(hd(getList(hd (tl treeList)))),[toAST(hd treeList),toAST(hd(tl (getList(hd (tl treeList)))))])
				
				|	"BoolG" => if (getList(hd (tl treeList))=[Node("EPSILON",[])]) then toAST(hd treeList)
								else Node(getroot(hd(getList(hd (tl treeList)))),[toAST(hd treeList),toAST(hd(tl (getList(hd (tl treeList)))))])
				
				|	"BoolH" => if (getList(hd (tl treeList))=[Node("EPSILON",[])]) then toAST(hd treeList)
								else Node(getroot(hd(getList(hd (tl treeList)))),[toAST(hd treeList),toAST(hd(tl (getList(hd (tl treeList)))))])
				
				|	"BoolI"	=> 	if (length treeList)=1 then toAST(hd(treeList))
							else Node("NEG",[toAST(hd(tl treeList))])			
				
				|	"BoolJ" =>	toAST(hd treeList)
				
				|	"IntExpression" => if (getList(hd (tl treeList))=[Node("EPSILON",[])]) then toAST(hd treeList)
										else Node((getroot(hd(getList(hd (tl treeList))))),[toAST(hd treeList),toAST(hd(tl (getList(hd (tl treeList)))))])
				
				|	"IntT" =>  if (getList(hd (tl treeList))=[Node("EPSILON",[])]) then toAST(hd treeList)
								else Node(getroot(hd(getList(hd (tl treeList)))),[toAST(hd treeList),toAST(hd(tl (getList(hd (tl treeList)))))])
				
				|	"IntF" => if (length treeList)=1 then toAST(hd(treeList))
							else Node("UNMINUS",[toAST(hd(tl treeList))])
				
				|	"IntF1"	=> if (length treeList)=1 then toAST(hd treeList)
								else toAST(hd (tl treeList))			
				
				|	"Ident" => (hd treeList)
				
				|	"IntLiteral" => (hd treeList)
				
				|	"BoolLiteral" => (hd treeList)
				
				|	_ => Node(root,treeList);

fun printASTtofile (input,output) = (printTree(toAST(stringtoParseTree(tl (last (toTokenList(readFromFile(input),[],""))))),"",output);printStringtoFile("\n",output));

printASTtofile(hd list_arg,hd (tl list_arg));

OS.Process.exit(OS.Process.success);	