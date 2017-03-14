val list_arg = CommandLine.arguments();
datatype TOKEN =  OR 
				| EQ
				| NE
				| LT
				| GT
				| LP
				| RP
				| LB
				| RB
				| IF
				| LTE
				| GTE
				| EOS
				| NEG
				| AND
				| INT
				| BOOL
				| THEN
				| ELSE
				| PROC
				| READ
				| CALL
				| COMMA
				| WHILE
				| PRINT
				| ERROR of int*int
				| IDENT of string
				| BINADD
				| BINSUB
				| BINDIV
				| BINMUL
				| BINMOD
				| ASSIGN
				| INTLIT of int
				| BOOLVAL of string
				| UNMINUS
				| NULL;

fun extract (s:string, acc:string) = if (String.substring(s,size(s)-1,1)=",") then 
										(if acc = "" then acc else String.substring(acc,0,size(acc)-1))
									else extract(String.substring(s,0,size(s)-1),String.substring(s,size(s)-1,1)^acc);					

(*Last element from brackets*)
fun extractL (s:string, acc:string) = if (String.substring(s,size(s)-1,1)=",") then 
										(if acc = "" then acc else String.substring(acc,0,size(acc)-1))
									else extractL(String.substring(s,0,size(s)-1),String.substring(s,size(s)-1,1)^acc);

(*Second Last element when 3 in bracket*)
fun extractM (s:string, acc:string) = if (String.substring(s,size(s)-1,1)=",") then 
										(if acc = "" then acc else extractL(String.substring(s,0,size(s)-1)^")",""))
									else extractM(String.substring(s,0,size(s)-1),String.substring(s,size(s)-1,1)^acc);

fun extra (s:string, acc:string) = if (String.substring(s,size(s)-1,1)="(") then 
										(if acc = "" then acc else String.substring(acc,0,size(acc)-1))
									else extra(String.substring(s,0,size(s)-1),String.substring(s,size(s)-1,1)^acc);

(*First element in bracket when 2*)
fun extractF2 (s:string, acc:string) = if (String.substring(s,size(s)-1,1)=",") then 
										(if acc = "" then acc else extra(String.substring(s,0,size(s)-1)^")",""))
									else extractF2(String.substring(s,0,size(s)-1),String.substring(s,size(s)-1,1)^acc);										

(*First element in bracket when 3*)
fun extractF (s:string, acc:string) = if (String.substring(s,size(s)-1,1)=",") then 
										(if acc = "" then acc else extractF2(String.substring(s,0,size(s)-1)^")",""))
									else extractF(String.substring(s,0,size(s)-1),String.substring(s,size(s)-1,1)^acc);


fun toToken (s:string) = case String.substring(s,0,3) of
			"OR(" => OR
		| 	"EQ(" => EQ
		| 	"NE(" => NE
		| 	"LT(" => LT
		| 	"GT(" => GT
		| 	"LP(" => LP
		| 	"RP(" => RP
		| 	"LB(" => LB
		| 	"RB(" => RB
		| 	"IF(" => IF
		| 	"LTE" => LTE
		| 	"GTE" => GTE
		| 	"EOS" => EOS
		| 	"NEG" => NEG
		| 	"AND" => AND
		| 	"INT" => if (String.substring(s,0,4)="INT(") then INT else INTLIT(trunc(Option.getOpt(Real.fromString(extract(String.substring(s,0,size(s)-1),"")),0.0)))
		| 	"BOO" => if (String.substring(s,0,5)="BOOL(") then BOOL 
						else (if (extract(String.substring(s,0,size(s)-1),"")="true") then BOOLVAL("tt") else BOOLVAL("ff"))
		| 	"THE" => THEN
		| 	"ELS" => ELSE
		| 	"PRO" => PROC
		| 	"REA" => READ
		| 	"CAL" => CALL
		| 	"COM" => COMMA
		| 	"WHI" => WHILE
		| 	"PRI" => PRINT
		| 	"ERR" => ERROR(trunc(Option.getOpt(Real.fromString(extractF(String.substring(s,0,size(s)-1),"")),0.0)),trunc(Option.getOpt(Real.fromString(extractM(String.substring(s,0,size(s)-1),"")),0.0)))
		| 	"IDE" => IDENT(extract(String.substring(s,0,size(s)-1),""))
		| 	"BIN" => (case (String.substring(s,0,6)) of
						"BINADD" => BINADD
					|	"BINSUB" => BINSUB
					| 	"BINDIV" => BINDIV
					| 	"BINMUL" => BINMUL
					| 	"BINMOD" => BINMOD
					| 	_ => NULL)
		| 	"ASS" => ASSIGN
		| 	"UNM" => UNMINUS
		|	_	=> NULL;

 fun toString (t:TOKEN) = case t of
 				  OR	=> "OR"
				| EQ 	=> "EQ"
				| NE 	=> "NE"
				| LT 	=> "LT"
				| GT 	=> "GT"
				| LP 	=> "LP"
				| RP 	=> "RP"
				| LB 	=> "LB"
				| RB 	=> "RB"
				| IF 	=> "IF"
				| LTE 	=> "LTE"
				| GTE 	=> "GTE"
				| EOS 	=> "EOS"
				| NEG 	=> "NEG"
				| AND 	=> "AND"
				| INT 	=> "INT"
				| BOOL 	=> "BOOL"
				| THEN 	=> "THEN"
				| ELSE 	=> "ELSE"
				| PROC 	=> "PROC"
				| READ 	=> "READ"
				| CALL 	=> "CALL"
				| COMMA => "COMMA"
				| WHILE => "WHILE"
				| PRINT => "PRINT"
				| ERROR(a,b) => "ERROR at (" ^ Int.toString a ^ "," ^ Int.toString b ^ ")"
				| IDENT(b) => "Ident[" ^ b ^ "]"
				| BINADD => "BINADD"
				| BINSUB => "BINSUB"
				| BINDIV => "BINDIV"
				| BINMUL => "BINMUL"
				| BINMOD => "BINMOD"
				| ASSIGN => "ASSIGN"
				| INTLIT(a) => "IntLiteral[" ^ Int.toString a ^ "]"
				| BOOLVAL(a) => "BoolLiteral[" ^ a ^ "]"
				| UNMINUS => "UNMINUS"
				| _ => "NULL" ;

datatype SYMBOL = KEYWORD of string * string
				| INTSYMBOL of string * string
				| BOOLSYMBOL of string * string
				| PROCSYMBOL of string * string;

fun toStringSym (t:SYMBOL) =
    case t of
          KEYWORD(a,b) => a ^"\t" ^b ^"\n"
        | INTSYMBOL(a,scope) => a ^"\tIDENT\tINT\t"^scope^"\n"
        | BOOLSYMBOL(a,scope) => a ^"\tIDENT\tBOOL\t"^scope^"\n"
        | PROCSYMBOL(a,scope) => a ^"\tIDENT\tPROC\t"^scope^"\n";

fun isMember (s:TOKEN,[]) = false
	| isMember (s,(x::xs):TOKEN list) = if (s=x) then true else isMember(s,xs);

fun readfile (infile:string,outfile:string,outfileSymbol:string) = 
	let
		val ins = TextIO.openIn infile
        
        fun printStringtoFile(str:string) = (let 
										      	val f = TextIO.openAppend outfile
											in
												(TextIO.output (f, str); TextIO.closeOut f) 
										    end	)

        and printSymboltoFile(str:string) = (let 
										      	val f = TextIO.openAppend outfileSymbol
											in
												(TextIO.output (f, str); TextIO.closeOut f) 
										    end	)

        fun printKeyWords () = (printSymboltoFile(toStringSym(KEYWORD("int","INT")));
        						printSymboltoFile(toStringSym(KEYWORD("bool","BOOL")));
        						printSymboltoFile(toStringSym(KEYWORD("tt","BOOLVAL")));
        						printSymboltoFile(toStringSym(KEYWORD("ff","BOOLVAL")));
        						printSymboltoFile(toStringSym(KEYWORD("if","IF")));
        						printSymboltoFile(toStringSym(KEYWORD("then","THEN")));
        						printSymboltoFile(toStringSym(KEYWORD("else","ELSE")));
        						printSymboltoFile(toStringSym(KEYWORD("while","WHILE")));
        						printSymboltoFile(toStringSym(KEYWORD("proc","PROC")));
        						printSymboltoFile(toStringSym(KEYWORD("print","PRINT")));
        						printSymboltoFile(toStringSym(KEYWORD("read","READ")));
        						printSymboltoFile(toStringSym(KEYWORD("call","CALL")))
        						)

        and consume (token:TOKEN) = (printStringtoFile(toString(token)^" "); 
        							(case (TextIO.inputLine ins) of 
        								SOME (chunk) => toToken(chunk)
									|	NONE => (TextIO.closeIn ins; toToken("NULL")))
        							)

		and program(token:TOKEN) = (printStringtoFile("[Program\n\t[ ") ;
									printKeyWords();
		        					block(token,"global") ;
		        					printStringtoFile("\t]\n]\n"))

        and block(token:TOKEN,str:string) = (printStringtoFile("Block\n\t\t[ ") ;
        						let 
	        						val t = declarationSeq(token,str)
	        					in
	        						(printStringtoFile(", ");
	        						(let 
	        							val p = commandseq(t)
	        						in
	        							(printStringtoFile("\t\t]\n");p)		
	        						end))
	        					end) 	
		
		and declarationSeq(token:TOKEN,str:string) = (printStringtoFile("DeclarationSeq\n\t\t\t[ ") ;
								let 
									val t = varDecls(token,str)
								in
									(printStringtoFile(", ");
									(let 
		        							val p = procDecls(t,str)
		        						in
		        							(printStringtoFile("\t\t\t]\n\t\t");p)		
		        						end))
									end)

		and varDecls(token:TOKEN,str:string) = (printStringtoFile("VarDecls\n\t\t\t\t[ ");
								let
									val t = intVarDecls(token,str)
								in
									(printStringtoFile(", ");
									(let 
	        							val p = boolVarDecls(t,str)
	        						in
	        							(printStringtoFile("\t\t\t\t]\n\t\t\t");p)		
	        						end))
								end)	

		and intVarDecls(token:TOKEN,str) = (printStringtoFile("IntVarDecls\n\t\t\t\t\t[ ");
										(case token of
											INT => (let
														val t = consume(token)
													in
														(printStringtoFile(", ");
														(let 
						        							val p = varDef(t,str,0)
						        						in
						        							(printStringtoFile("]\n\t\t\t\t");p)		
						        						end))
													end)
										|   _ => (printStringtoFile("EPSILON ");printStringtoFile("],\n\t\t\t\t");token))
										)

		and boolVarDecls(token:TOKEN,str) = (printStringtoFile("BoolVarDecls\n\t\t\t\t\t[ ");
										(case token of
											BOOL => (let
														val t = consume(token)
													in
														(printStringtoFile(", ");
														(let 
															val p = varDef(t,str,1)
														in
															(printStringtoFile("]\n");p)		
														end))
													end) 
										|	_ 	=> (printStringtoFile("EPSILON ");printStringtoFile("]\n");token))
										)	
		
		and varDef(token:TOKEN,str:string,intOrBool:int) = (*0 for int / 1 for bool*)
									(printStringtoFile("VarDef [ ");
									(case token of
										IDENT(a) => (let
														val t = consume(token)
													in
														(printStringtoFile(", ");
														if intOrBool = 0 then printSymboltoFile(toStringSym(INTSYMBOL(a,str)))
															else printSymboltoFile(toStringSym(BOOLSYMBOL(a,str)));
														(let 
						        							val p = varDef1(t,str,intOrBool)
						        						in
						        							(printStringtoFile("] ");p)		
						        						end))
													end)
									|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
									|	_ => (printStringtoFile("Error ");printStringtoFile("] ");token))
									)

		and varDef1(token:TOKEN,str:string,intOrBool:int) = 
									(printStringtoFile("VarDef1 [ ");
										(case token of
											COMMA => (let
														val t = consume(token)
													in
														(printStringtoFile(", ");
														(let 
						        							val p = varDef(t,str,intOrBool)
						        						in
						        							(printStringtoFile("] ");p)		
						        						end))
													end)
										|	EOS => (let 
					        							val p = consume(token)
					        						in
					        							(printStringtoFile("] ");p)		
					        						end)
										|	ERROR(a,b) => (printStringtoFile(toString(token));token)
										|	_ => (printStringtoFile("Error ");printStringtoFile("] ");token))
									)	

		and procDecls(token:TOKEN,str:string) = (printStringtoFile("ProcDecls\n\t\t\t\t[ "); 
									(case token of
										PROC =>	(let val t = consume(token) in
													(printStringtoFile(", ");
													(case t of
														IDENT(a) => (let val t1 = consume(t) in
																		(printStringtoFile(", ");
																		printSymboltoFile(toStringSym(PROCSYMBOL(a,str)));
																		(let val t2 = block(t1,str^":"^a) in
																			(printStringtoFile(", ");
																			(case t2 of
																				EOS => (let
																							val t3 = consume(t2)
																						in
																							(printStringtoFile(", ");
																							(let 
															        							val p = procDecls(t3,str)
															        						in
															        							(printStringtoFile("\t\t\t]\n");p)		
															        						end))
																						end)
																			|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
																			|	_ => (printStringtoFile("Error ");printStringtoFile("\t\t\t]\n");token)		
																				))
																		end))		
																	end)
													|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
													|	_ => (printStringtoFile("Error ");printStringtoFile("]\n");token)		
														))
												end) 
									|	_	=> (printStringtoFile("EPSILON ");printStringtoFile("]\n");token))
									)

		and commandseq(token:TOKEN) =(printStringtoFile("CommandSeq\n\t\t\t[ ");
								(case token of
									LB => (let val t = consume(token) in
											(printStringtoFile(", ");
											(let val t1 = command(t) in 
												(printStringtoFile(", ");
												(case t1 of 
													RB => (let 
							        							val p = consume(t1)
							        						in
							        							(printStringtoFile("]\n");p)		
							        						end)
												|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
												|	_ => (printStringtoFile("Error ");printStringtoFile("]\n");t1)
												))
											end))
										end)
								|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
								|	_ => (printStringtoFile("Error ");printStringtoFile("]\n");token))
								)

		and command(token:TOKEN) = (printStringtoFile("Command [ ");
									(case token of
										IDENT(a) => (let val t = assignmentCmd(token) in
														(printStringtoFile(", ");
														(case t of
															EOS => (let
																		val t1 = consume(t)
																	in
																		(printStringtoFile(", ");
																		(let 
										        							val p = command(t1)
										        						in
										        							(printStringtoFile("] ");p)		
										        						end))
																	end)
														|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
														|	_ => (printStringtoFile("Error ");printStringtoFile("] ");t))
														)
													end)
									|	CALL	=>	(let val t = callCmd(token) in
														(printStringtoFile(", ");
														(case t of
															EOS => (let
																		val t1 = consume(t)
																	in
																		(printStringtoFile(", ");
																		(let 
										        							val p = command(t1)
										        						in
										        							(printStringtoFile("] ");p)		
										        						end))
																	end)
														|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
														|	_ => (printStringtoFile("Error ");printStringtoFile("] ");t))
														)
													end)
									|	READ 	=>	(let val t = readCmd(token) in
														(printStringtoFile(", ");
														(case t of
															EOS => (let
																		val t1 = consume(t)
																	in
																		(printStringtoFile(", ");
																		(let 
										        							val p = command(t1)
										        						in
										        							(printStringtoFile("] ");p)		
										        						end))
																	end)
														|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
														|	_ => (printStringtoFile("Error ");printStringtoFile("] ");t))
														)
													end)
									|	PRINT 	=>	(let val t = printCmd(token) in
														(printStringtoFile(", ");
														(case t of
															EOS => (let
																		val t1 = consume(t)
																	in
																		(printStringtoFile(", ");
																		(let 
										        							val p = command(t1)
										        						in
										        							(printStringtoFile("] ");p)		
										        						end))
																	end)
														|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
														|	_ => (printStringtoFile("Error ");printStringtoFile("] ");t))
														)
													end)
									|	IF 		=>	(let val t = conditionalCmd(token) in
														(printStringtoFile(", ");
														(case t of
															EOS => (let
																		val t1 = consume(t)
																	in
																		(printStringtoFile(", ");
																		(let 
										        							val p = command(t1)
										        						in
										        							(printStringtoFile("] ");p)		
										        						end))
																	end)
														|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
														|	_ => (printStringtoFile("Error ");printStringtoFile("] ");t))
														)
													end)
									|	WHILE 	=>	(let val t = whileCmd(token) in
														(printStringtoFile(", ");
														(case t of
															EOS => (let
																		val t1 = consume(t)
																	in
																		(printStringtoFile(", ");
																		(let 
										        							val p = command(t1)
										        						in
										        							(printStringtoFile("] ");p)		
										        						end))
																	end)
														|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
														|	_ => (printStringtoFile("Error ");printStringtoFile("] ");t))
														)
													end)
									|	ERROR(a,b) => (printStringtoFile(toString(token));token)
									|	_		=>	(printStringtoFile("EPSILON ");printStringtoFile("] ");token)
									))


		and assignmentCmd(token:TOKEN) = (printStringtoFile("AssignmentCmd [ ");
										(case token of
											IDENT(a) => (let val t = consume(token) in
													(printStringtoFile(", ");
													(case t of
														ASSIGN => (let
																	val t1 = consume(t)
																in 
																	(printStringtoFile(", ");
																	(let 
																		val p = expression(t1)
																	in
																		(printStringtoFile("] ");p)		
																	end))																
																end)
													|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
													|	_  => (printStringtoFile("Error ");printStringtoFile("] ");t))
													)
												end)
										|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
										|	_ => (printStringtoFile("Error ");printStringtoFile("] ");token)	
										))

		and callCmd(token:TOKEN) = (printStringtoFile("CallCmd [ ");
										(case token of
											CALL => (let
														val	t = consume(token) 
													in
														(printStringtoFile(", ");
														(case t of
															IDENT(a) => (let 
																			val p = consume(t)
																		in
																			(printStringtoFile("] ");p)		
																		end)
														|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
														|	_  => (printStringtoFile("Error ");printStringtoFile("] ");t))
														)
													end)
										|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
										|	_ => (printStringtoFile("Error ");printStringtoFile("] ");token)	
										))

		and readCmd(token:TOKEN) = (printStringtoFile("ReadCmd [ ");
										(case token of
											READ => (let val t = consume(token) in
													(printStringtoFile(", ");
													(case t of
														LP => (let val t1 = consume(t) in 
																(printStringtoFile(", ");
																(case t1 of
																	IDENT(a) => (let val t2 = consume(t1) in
																				(printStringtoFile(", ");
																				(case t2 of
																					RP => (let 
															        							val p = consume(t2)
															        						in
															        							(printStringtoFile("] ");p)		
															        						end)
																				|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
																				|	_  => (printStringtoFile("Error ");printStringtoFile("] ");t2))
																				)
																			end)
																|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
																|	_  => (printStringtoFile("Error ");printStringtoFile("] ");t1))
																)
															end)
													|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
													|	_  => (printStringtoFile("Error ");printStringtoFile("] ");t))
													)
												end)
										|	ERROR(a,b) => (printStringtoFile(toString(token));token)
										|	_ => (printStringtoFile("Error ");printStringtoFile("] ");token)	
										))

		and printCmd(token:TOKEN) = (printStringtoFile("PrintCmd [ ");
										(case token of
											PRINT => (let val t = consume(token) in
													(printStringtoFile(", ");
													(case t of
														LP => (let val t1 = consume(t) in 
																(printStringtoFile(", ");
																(case t1 of
																	IDENT(a) => (let val t2 = consume(t1) in
																				(printStringtoFile(", ");
																				(case t2 of
																					RP => 	(let 
															        							val p = consume(t2)
															        						in
															        							(printStringtoFile("] ");p)		
															        						end)
																				|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
																				|	_  => (printStringtoFile("Error ");printStringtoFile("] ");t2))
																				)
																			end)
																|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
																|	_  => (printStringtoFile("Error ");printStringtoFile("] ");t1))
																)
															end)
													|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
													|	_  => (printStringtoFile("Error ");printStringtoFile("] ");t))
													)
												end)
										|	ERROR(a,b) => (printStringtoFile(toString(token));token)
										|	_ => (printStringtoFile("Error ");printStringtoFile("] ");token)	
										))														

		and conditionalCmd(token:TOKEN) = (printStringtoFile("ConditionalCmd [ ");
										(case token of
											IF => (let val t = consume(token) in
													(printStringtoFile(", ");
													(let val t1 = boolExpression(t) in
														(printStringtoFile(", ");
														(case t1 of
															THEN => (let val t2 = consume(t1) in
																		(printStringtoFile(", ");
																		(let val t3 = commandseq(t2) in
																			(printStringtoFile(", ");
																			(case t3 of
																				ELSE => (let 
																							val t4 = consume(t3)
																						in 
																							(printStringtoFile(", ");
																							(let 
															        							val p = commandseq(t4)
															        						in
															        							(printStringtoFile("] ");p)		
															        						end))
																						end)
																			|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
																			|	_  => (printStringtoFile("Error ");printStringtoFile("] ");t3)
																			))
																		end))
																	end)
														|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
														|	_  => (printStringtoFile("Error ");printStringtoFile("] ");t1)
														))
													end))
												 end)
										|	ERROR(a,b) => (printStringtoFile(toString(token));token)
										|	_ => (printStringtoFile("Error ");printStringtoFile("] ");token)	
										))

		and whileCmd(token:TOKEN) = (printStringtoFile("WhileCmd [ ");
										(case token of
											WHILE => (let
														val t = consume(token)
													in 
														(printStringtoFile(", ");
														(let 
															val t1 = boolExpression(t) 
														in
															(printStringtoFile(", ");
															(let 
							        							val p = commandseq(t1)
							        						in
							        							(printStringtoFile("] ");p)		
							        						end))
														end))
													end)
										|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
										|	_ => (printStringtoFile("Error ");printStringtoFile("] ");token)	
										))

		and expression(token:TOKEN) = (printStringtoFile("Expression [ ");
										(let 
		        							val p = boolExpression(token)
		        						in
		        							(printStringtoFile("] ");p)		
		        						end))

		and intExpression(token:TOKEN) = (printStringtoFile("IntExpression [ ");
											(let
												val t = intT(token)
											in
												(printStringtoFile(", ");
												(let 
				        							val p = intE(t)
				        						in
				        							(printStringtoFile("] ");p)		
				        						end))	
											end))
		
		and intE(token:TOKEN) = (printStringtoFile("IntE [ ");
								(case token of
									BINADD => (let
												val t = consume(token)
											in
												(printStringtoFile(", ");
												(let 
				        							val p = intExpression(t)
				        						in
				        							(printStringtoFile("] ");p)		
				        						end))
											end)
								|	BINSUB => (let
												val t = consume(token) 
											in 
												(printStringtoFile(", ");
												(let 
				        							val p = intExpression(t)
				        						in
				        							(printStringtoFile("] ");p)		
				        						end))
											end) 
								|	ERROR(a,b) => (printStringtoFile(toString(token));token)
								|	_ 		=> (printStringtoFile("EPSILON ");printStringtoFile("] ");token))
								)

		and intT(token:TOKEN) = (printStringtoFile("IntT [ ");
								(let
									val t = intF(token)
								in
									(printStringtoFile(", ");
									(let 
	        							val p = intT1(t)
	        						in
	        							(printStringtoFile("] ");p)		
	        						end))	
								end))

		and intT1 (token:TOKEN) = (printStringtoFile("IntT1 [ ");
								(case token of
									BINMOD => (let
												val t = consume(token) 
											in
												(printStringtoFile(", ");
												(let 
				        							val p = intT(t)
				        						in
				        							(printStringtoFile("] ");p)		
				        						end))
											end)
								|	BINMUL => (let
												val t = consume(token) 
											in
												(printStringtoFile(", ");
												(let 
				        							val p = intT(t)
				        						in
				        							(printStringtoFile("] ");p)		
				        						end))
											end)
								|	BINDIV => (let
												val t = consume(token) 
											in
												(printStringtoFile(", ");
												(let 
				        							val p = intT(t)
				        						in
				        							(printStringtoFile("] ");p)		
				        						end))
											end)
								|	ERROR(a,b) => (printStringtoFile(toString(token));token)			 	
								|	_ 		=> (printStringtoFile("EPSILON ");printStringtoFile("] ");token))
								)

		and intF (token:TOKEN) = (printStringtoFile("IntF [ ");
								(case token of
									UNMINUS => (let
													val t = consume(token) 
												in
													(printStringtoFile(", ");
													(let 
					        							val p = intF1(t)
					        						in
					        							(printStringtoFile("] ");p)		
					        						end)) 
												end)
								|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
								|	_ 		=> (let 
				        							val p = intF1(token)
				        						in
				        							(printStringtoFile("] ");p)		
					        					end))
								)

		and intF1 (token:TOKEN) = (printStringtoFile("IntF1 [ ");
								(case token of
									IDENT(a) => (let 
				        							val p = consume(token)
				        						in
				        							(printStringtoFile("] ");p)		
					        					end)
								|	INTLIT(a) => (let 
				        							val p = consume(token)
				        						in
				        							(printStringtoFile("] ");p)
					        					end)
								|	LP => (let val t = consume(token) in 
											(printStringtoFile(", ");
											(let val t1 = boolExpression(t) in
												(printStringtoFile(", ");
												(case t1 of 
													RP => (let 
							        							val p = consume(t1)
							        						in
							        							(printStringtoFile("] ");p)		
								        					end)
												|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
												|	_  => (printStringtoFile("Error ");printStringtoFile("] ");t1)
												))
											end))
										end)
								|	ERROR(a,b) => (printStringtoFile(toString(token));token)
								|	_ => (printStringtoFile("Error ");printStringtoFile("] ");token)
								)
								)

		and boolExpression (token:TOKEN) = (printStringtoFile("BoolExpression [ ");
											(let
												val t = boolF(token)
											in
												(printStringtoFile(", ");
												(let 
				        							val p = boolE(t)
				        						in
				        							(printStringtoFile("] ");p)
					        					end))	
											end))

		and boolE (token:TOKEN) = (printStringtoFile("BoolE [ ");
								(case token of
									OR => (let
												val t = consume(token)
											in
												(printStringtoFile(", ");
												(let 
				        							val p = boolExpression(t)
				        						in
				        							(printStringtoFile("] ");p)
					        					end)) 
											end)
								|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
								|	_ 		=> (printStringtoFile("EPSILON ");printStringtoFile("] ");token))
								)

		and boolF (token:TOKEN) = (printStringtoFile("BoolF [ ");
									(let
										val t = boolG(token)
									in
										(printStringtoFile(", ");
										(let 
		        							val p = boolF1(t)
		        						in
		        							(printStringtoFile("] ");p)
			        					end))	
									end))

		and boolF1 (token:TOKEN) = (printStringtoFile("BoolF1 [ ");
								(case token of
									AND => (let
												val t = consume(token) 
											in
												(printStringtoFile(", ");
												(let 
				        							val p = boolF(t)
				        						in
				        							(printStringtoFile("] ");p)
					        					end))
											end)
								|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
								|	_ 	=> (printStringtoFile("EPSILON ");printStringtoFile("] ");token))
								)

		and boolG (token:TOKEN) = (printStringtoFile("BoolG [ ");
									(let
										val t = boolH(token)
									in
										(printStringtoFile(", ");
										(let 
		        							val p = boolG1(t)
		        						in
		        							(printStringtoFile("] ");p)
			        					end))	
									end))

		and boolG1 (token:TOKEN) = (printStringtoFile("BoolG1 [ ");
								(case token of
									EQ => (	let 
												val t = consume(token) 
											in
												(printStringtoFile(", ");
												(let 
				        							val p = boolG(t)
				        						in
				        							(printStringtoFile("] ");p)
					        					end))
											end)
								|	NE => (	let 
												val t = consume(token) 
											in
												(printStringtoFile(", ");
												(let 
				        							val p = boolG(t)
				        						in
				        							(printStringtoFile("] ");p)
					        					end))
											end)
								|	ERROR(a,b) => (printStringtoFile(toString(token));token)
								|	_ 		=> (printStringtoFile("EPSILON ");printStringtoFile("] ");token))
								)

		and boolH (token:TOKEN) = (printStringtoFile("BoolH [ ");
									(let
										val t = boolI(token)
									in
										(printStringtoFile(", ");
										(let 
		        							val p = boolH1(t)
		        						in
		        							(printStringtoFile("] ");p)
			        					end))
									end))

		and boolH1 (token:TOKEN) = (printStringtoFile("BoolH1 [ ");
								(case token of
									LT => (	let 
												val t = consume(token) 
											in 
												(printStringtoFile(", ");
												(let 
				        							val p = boolH(t)
				        						in
				        							(printStringtoFile("] ");p)
					        					end))
											end)
								|	LTE => (let 
												val t = consume(token) 
											in 
												(printStringtoFile(", ");
												(let 
				        							val p = boolH(t)
				        						in
				        							(printStringtoFile("] ");p)
					        					end))
											end)
								|	GT => (	let 
												val t = consume(token) 
											in 
												(printStringtoFile(", ");
												(let 
				        							val p = boolH(t)
				        						in
				        							(printStringtoFile("] ");p)
					        					end))
											end)
								|	GTE => (let 
												val t = consume(token) 
											in 
												(printStringtoFile(", ");
												(let 
				        							val p = boolH(t)
				        						in
				        							(printStringtoFile("] ");p)
					        					end))
											end)
								|	ERROR(a,b) => (printStringtoFile(toString(token));token)
								|	_ 		=> (printStringtoFile("EPSILON ");printStringtoFile("] ");token))
								)

		and boolI (token:TOKEN) = (printStringtoFile("BoolI [ ");
								(case token of
									NEG => (let 
												val t = consume(token)
											in 
												(printStringtoFile(", ");
												(let 
				        							val p = boolJ(t)
				        						in
				        							(printStringtoFile("] ");p)
					        					end)) 
											end)
								|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
								|	_ 	=> (let 
			        							val p = boolJ(token)
			        						in
			        							(printStringtoFile("] ");p)
				        					end))
								)

		and boolJ (token:TOKEN) = (printStringtoFile("BoolJ [ ");
								(case token of
									BOOLVAL(a) => (	let 
					        							val p = consume(token)
					        						in
					        							(printStringtoFile("] ");p)
						        					end)
								|	ERROR(a,b) => (printStringtoFile(toString(token));token)	
								|	_ => (let 
			        							val p = intExpression(token)
			        						in
			        							(printStringtoFile("] ");p)
				        					end))
								)


	in
		case TextIO.inputLine ins of
			SOME(v) => program(toToken(v))
		|	NONE => TextIO.closeIn ins	
	end;

readfile(hd (list_arg),hd (tl list_arg),hd (tl (tl list_arg)));
OS.Process.exit(OS.Process.success);