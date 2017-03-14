val list_arg = CommandLine.arguments();
datatype TOKEN = UNMINUS of int * int
				| BINADD of int * int
				| BINSUB of int * int
				| BINDIV of int * int
				| BINMUL of int * int
				| BINMOD of int * int
				| NEG of int * int
				| AND of int * int
				| OR of int * int
				| ASSIGN of int * int
				| EQ of int * int
				| NE of int * int
				| LT of int * int
				| LTE of int * int
				| GT of int * int
				| GTE of int * int
				| LP of int * int
				| RP of int * int
				| LB of int * int
				| RB of int * int
				| EOS of int * int
				| COMMA of int * int
				| INT of int * int
				| BOOL of int * int
				| IF of int * int
				| THEN of int * int
				| ELSE of int * int
				| WHILE of int * int
				| PROC of int * int
				| PRINT of int * int
				| READ of int * int
				| ERROR of int * int * int
				| INTLIT of int * int * int
				| IDENT of int * int * string
				| BOOLVAL of int * int * bool;
datatype State = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve | Thirteen | Fourteen | Fifteen | Sixteen | Seventeen | Eighteen | Ninteen | Twenty;
fun toString TOKEN = case TOKEN of UNMINUS(a,b) => "UNMINUS(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
								| BINADD(a,b) => "BINADD(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| BINSUB(a,b) => "BINSUB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| BINDIV(a,b) => "BINDIV(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| BINMUL(a,b) => "BINMUL(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| BINMOD(a,b) => "BINMOD(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| NEG(a,b) => "NEG(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| AND(a,b) => "AND(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| OR(a,b) => "OR(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| ASSIGN(a,b) => "ASSIGN(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| EQ(a,b) => "EQ(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| NE(a,b) => "NE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| LT(a,b) => "LT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| LTE(a,b) => "LTE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| GT(a,b) => "GT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| GTE(a,b) => "GTE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| LP(a,b) => "LP(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| RP(a,b) => "RP(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| LB(a,b) => "LB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
								| RB(a,b) => "RB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
								| EOS(a,b) => "EOS(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| COMMA(a,b) => "COMMA(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| INT(a,b) => "INT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| BOOL(a,b) => "BOOL(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| IF(a,b) => "IF(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| THEN(a,b) => "THEN(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| ELSE(a,b) => "ELSE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| WHILE(a,b) => "WHILE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| PROC(a,b) => "PROC(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| PRINT(a,b) => "PRINT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| READ(a,b) => "READ(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n" 
								| ERROR(a,b,c)=>"ERROR(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Int.toString c ^")\n" 
								| INTLIT(a,b,c)=>"INTLIT(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Int.toString c ^")\n" 
								| IDENT(a,b,c)=>"IDENT(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ c ^ ")\n" 
								| BOOLVAL(a,b,c)=>"BOOLVAL("^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Bool.toString c ^")\n";
fun isMember (s:string,[]) = false
	| isMember (s,(x::xs):string list) = if (s=x) then true else isMember(s,xs);
fun printError(acc:string,row:int,col:int) = toString(ERROR(row,(col-size(acc)),size(acc)));
fun printSym(acc:string,row:int,col:int) = 
	case acc of
			"~" => toString(UNMINUS(row,col-size(acc))) 
		| 	"+" => toString(BINADD(row,col-size(acc)))
		| 	"-" => toString(BINSUB(row,col-size(acc)))
		| 	"/" => toString(BINDIV(row,col-size(acc)))
		| 	"*" => toString(BINMUL(row,col-size(acc)))
		| 	"%" => toString(BINMOD(row,col-size(acc)))
		| 	"!" => toString(NEG(row,col-size(acc)))
		| 	"&&" => toString(AND(row,col-size(acc)))
		| 	"||" => toString(OR(row,col-size(acc)))
		| 	":=" => toString(ASSIGN(row,col-size(acc)))
		| 	"=" => toString(EQ(row,col-size(acc)))
		| 	"<>" => toString(NE(row,col-size(acc)))
		| 	"<" => toString(LT(row,col-size(acc)))
		| 	"<=" => toString(LTE(row,col-size(acc)))
		| 	">" => toString(GT(row,col-size(acc)))
		| 	">=" => toString(GTE(row,col-size(acc)))
		| 	"(" => toString(LP(row,col-size(acc)))
		| 	")" => toString(RP(row,col-size(acc)))
		| 	"{" => toString(LB(row,col-size(acc)))
		| 	"}" => toString(RB(row,col-size(acc)))
		| 	";" => toString(EOS(row,col-size(acc)))
		|	"," => toString(COMMA(row,col-size(acc)))
		|	_ => "";
fun printNum (acc:string,row:int,col:int) = toString(INTLIT(row,col-size(acc),trunc(Option.getOpt(Real.fromString(acc),0.0))));
fun printIdent (acc:string,row:int,col:int) = 
	case acc of
		 	"int" => toString(INT(row,col-size(acc)))
		| 	"bool" => toString(BOOL(row,col-size(acc)))
		| 	"if" => toString(IF(row,col-size(acc)))
		| 	"then" => toString(THEN (row,col-size(acc)))
		| 	"else" => toString(ELSE (row,col-size(acc)))
		| 	"while" => toString(WHILE (row,col-size(acc)))
		| 	"proc" => toString(PROC (row,col-size(acc)))
		| 	"print" => toString(PRINT (row,col-size(acc)))
		| 	"read" => toString(READ (row,col-size(acc)))
		| 	"tt" => toString(BOOLVAL (row,col-size(acc),true))
		| 	"ff" => toString(BOOLVAL (row,col-size(acc),false))
		|	_   => toString(IDENT(row,col-size(acc),acc));
val outfile = hd (tl list_arg);
fun printStringtoFile (str:string, file:string) = 
    let 
      	val f =  TextIO.openAppend file
    in
    	(TextIO.output (f, str); TextIO.closeOut f) 
    end	;	
fun scan1(flag, charlist , acc : string , State , row:int , row1:int , col1:int , col:int) = 
	case charlist of
		[] => printStringtoFile("",outfile)
	|	y::xs  => (
			case y of
				#"\n" => (if flag = false then scan1(true,#" "::charlist,acc,State,row,row1,col1,col) else scan1(false,xs,acc,State,row+1,row1,col1,1))
			|	#"\t" => scan1(flag,#" "::xs,acc,State,row,row1,col1,col)
			|	_	=>(
				let
					val x = Char.toString(y);
		 		in
		 		case State of

					One => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then scan1(flag,xs,acc^x,Two,row,row1,col1,col+1)
						else if (x>="0" andalso x<="9") then scan1(flag,xs,acc^x,Fourteen,row,row1,col1,col+1)
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then scan1(flag,xs,acc^x,Fifteen,row,row1,col1,col+1)	
						else (case String.sub(x,0) of
							#"&" => scan1(flag,xs,acc^x,Three,row,row1,col1,col+1)
						|	#"|" => scan1(flag,xs,acc^x,Five,row,row1,col1,col+1)
						|	#":" => scan1(flag,xs,acc^x,Seven,row,row1,col1,col+1)
						|	#"<" => scan1(flag,xs,acc^x,Nine,row,row1,col1,col+1)
						|	#">" => scan1(flag,xs,acc^x,Twelve,row,row1,col1,col+1)
						|	#"(" => scan1(flag,xs,acc^x,Seventeen,row,row1,col1,col+1)
						|	#" " => scan1(flag,xs,acc,One,row,row1,col1,col+1)
						|	_	=> scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1))

				|	Two => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fourteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fifteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printSym (acc,row,col),outfile); scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))

				|	Three => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"&" => scan1(flag,xs,acc^x,Four,row,row1,col1,col+1)
						|	#"|" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1))


				|	Four => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fourteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fifteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printSym (acc,row,col),outfile); scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))

				|	Five => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Four,row,row1,col1,col+1))
						|	#"|" => (scan1(flag,xs,acc^x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1))

				|	Six => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fourteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fifteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printSym (acc,row,col),outfile); scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))

				|	Seven => if isMember(x,["~","+","-","/","*","%",")","{","}","!",";",","]) then (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"=" => scan1(flag,xs,acc^x,Eight,row,row1,col1,col+1)
						|	#"&" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Four,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1))

				|	Eight => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fourteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fifteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printSym (acc,row,col),outfile); scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))

				|	Nine => if isMember(x,["~","+","-","/","*","%",")","{","}","!",";",","]) then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fourteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fifteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"=" => scan1(flag,xs,acc^x,Eleven,row,row1,col1,col+1)	
						|	#"|" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => scan1(flag,xs,acc^x,Ten,row,row1,col1,col+1)
						|	#"(" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printSym (acc,row,col),outfile); scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))


				|	Ten => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fourteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fifteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printSym (acc,row,col),outfile); scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))

				|	Eleven => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fourteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fifteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printSym (acc,row,col),outfile); scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))

				|	Twelve => if isMember(x,["~","+","-","/","*","%",")","{","}","!",";",","]) then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fourteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fifteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"=" => scan1(flag,xs,acc^x,Thirteen,row,row1,col1,col+1)	
						|	#"|" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printSym (acc,row,col),outfile); scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))


				|	Thirteen => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fourteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fifteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printSym (acc,row,col),outfile); scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))

				|	Fourteen => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printNum(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then scan1(flag,xs,acc^x,Fourteen,row,row1,col1,col+1)
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1)	
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printNum(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printNum(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printNum(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printNum(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printNum(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printNum(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printNum (acc,row,col),outfile);scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printIdent(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))

				|	Fifteen => if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") orelse (x>="0" andalso x<="9") then scan1(flag,xs,acc^x,Fifteen,row,row1,col1,col+1)
						else if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printIdent(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printIdent(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printIdent(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printIdent(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printIdent(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printIdent(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printIdent(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printIdent(acc,row,col),outfile);scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printIdent(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))


				|	Sixteen => if isMember(x,["~","+","-","/","*","%",")","{","}","!","=",";",","]) then (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else (case String.sub(x,0) of
							#"&" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printError(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printError (acc,row,col),outfile);scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (scan1(flag,xs,acc^x,Sixteen,row,row1,col1,col+1)))

				|	Seventeen => if isMember(x,["~","+","-","/","%",")","{","}","!","=",";",","]) then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Two,row,row1,col1,col+1))
						else if (x>="0" andalso x<="9") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fourteen,row,row1,col1,col+1))
						else if (x>="a" andalso x<="z") orelse (x>="A" andalso x<="Z") then (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Fifteen,row,row1,col1,col+1))	
						else (case String.sub(x,0) of
							#"*" => scan1(flag,xs,acc^x,Eighteen,row,row,col-1,col+1)
						|	#"&" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Three,row,row1,col1,col+1))
						|	#"|" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Five,row,row1,col1,col+1))
						|	#":" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seven,row,row1,col1,col+1))
						|	#"<" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Nine,row,row1,col1,col+1))
						|	#">" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Twelve,row,row1,col1,col+1))
						|	#"(" => (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Seventeen,row,row1,col1,col+1))
						|	#" " => (printStringtoFile(printSym (acc,row,col),outfile); scan1(flag,xs,"",One,row,row1,col1,col+1))
						|	_	=> (printStringtoFile(printSym(acc,row,col),outfile);scan1(flag,xs,x,Sixteen,row,row1,col1,col+1)))

				|	Eighteen => if x="*" then scan1(flag,xs,acc^x,Ninteen,row,row1,col1,col+1)
						else if xs=[] then (printStringtoFile(printError (acc,row1,col1+size(acc)),outfile);scan1(flag,xs,acc^x,Eighteen,row,row1,col1,col+1))
						else scan1(flag,xs,acc^x,Eighteen,row,row1,col1,col+1)

				|	Ninteen => if x=")" then scan1(flag,xs,acc^x,Twenty,row,row1,col1,col+1)
						else if x="*" then scan1(flag,xs,acc^x,Ninteen,row,row1,col1,col+1)
						else if xs = [] then (printStringtoFile(printError (acc,row1,col1+size(acc)),outfile);scan1(flag,xs,acc^x,Eighteen,row,row1,col1,col+1))
						else scan1(flag,xs,acc^x,Eighteen,row,row1,col1,col+1)

				|	Twenty => scan1(flag,charlist,"",One,row,1,1,col)
							
				end)
			)
val infile = (hd list_arg);
fun readInput (filename:string) = 
    let
    	val f = TextIO.getInstream(TextIO.openIn filename);
		fun loop (clist, f) = case TextIO.StreamIO.input1 f of
			SOME (c, f') => loop (c::clist, f')
		  | NONE		 => (TextIO.StreamIO.closeIn; #" "::clist)
	in
		rev(loop ([], f))
    end;
scan1(false,readInput(infile),"",One,1,1,1,1);

OS.Process.exit(OS.Process.success);