(*use "bi.sml";*)

use "bics5140277.sml";
open BigInt;

val list_arg = CommandLine.arguments();

fun remove(chunk,acc) = if chunk = "" then acc
					else if (String.substring(chunk,0,1) = "\n" orelse String.substring(chunk,0,1) = "\r") then remove(String.substring(chunk,1,size(chunk)-1),acc)
					else remove(String.substring(chunk,1,size(chunk)-1),acc^String.substring(chunk,0,1));

fun readfile (filename:string) =
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (accum: string list, f) =
	    case (TextIO.StreamIO.inputLine f) of 
	        SOME(chunk, f') => loop (remove(chunk,"")::accum, f')
	      | NONE => (TextIO.StreamIO.closeIn f; accum)
            (* esac *)
    in  rev(loop ([], f))
    end;
(*readfile("input.txt");*)

fun isMember (s:string,[]) = false
	| isMember (s,(x::xs):string list) = if (s=x) then true else isMember(s,xs);

fun strToTuple (s) = let
						fun strToList (s:string,acc,l) = (if (s="") then l
								else if (String.substring(s,0,1)=" ") then strToList(String.substring(s,1,size(s)-1),"",l@acc::[])
								else if (size(s)=1) then strToList("","",l@(acc^s)::[])
								else strToList(String.substring(s,1,size(s)-1),acc^String.substring(s,0,1),l))

						val l = strToList(s,"",[])

					in
						(hd l,hd (tl l),hd (tl (tl l)),hd (tl (tl (tl l))))
					end;

fun printStringtoFile (str:string, file:string) = 
    let 
      	val f =  TextIO.openAppend file
    in
    	(TextIO.output (f, str); TextIO.closeOut f) 
    end	;

fun listoftuples ([],l) = l
|	listoftuples (x::xs,l) = listoftuples(xs,l@strToTuple(x)::[]);

fun printsymtable ([]) = print ""
|	printsymtable ((a,b)::xs) = (print (""^a^" : "^b^" || ");printsymtable(xs))
 
(*listoftuples(readfile("input.txt"),[]); *)

fun stack_push (a,l) = a::l;
fun stack_top ([]) = ""
|	stack_top (x::xs) = x;

fun get ([],a:string) = "Not found Exception raised"
|	get (x::xs:(string*string*string) list,a) = if ((#1 x)=a) then (#2 x)
										else get (xs,a);

fun gettype ([],a:string) = "Not found Exception raised"
|	gettype (x::xs:(string*string*string) list,a) = if ((#1 x)=a) then (#3 x)
										else gettype (xs,a);

fun put ([],a:string*string*string,l) = a::l
|	put (x::xs:(string*string*string) list,a,l) = if ((#1 x)=(#1 a)) then l@(a::xs)
										else put (xs,a,l@[x]);

exception Failure of string;

fun getnth([],i) = ("","","","")
|	getnth(x::xs,i)	= if (i<0) then ("","","","")
				else if (i=0) then x
				else getnth(xs,i-1);	

fun chomp1 s =  (* to remove newline character on reading *)
    let val charlist = rev (explode s)
        fun nibble [] = []
          | nibble (#"\n"::l) = l
          | nibble l = l
    in  implode (rev (nibble charlist))
    end;

fun isPosInt(s) = if s="" then true
				else if size(s)= 1 then isMember(s,["0","1","2","3","4","5","6","7","8","9"])
				else isMember(String.substring(s,0,1),["0","1","2","3","4","5","6","7","8","9"]) andalso isPosInt(String.substring(s,1,size(s)-1));

fun isInt(s) = if s="" then false
				else if size(s)=1 then isPosInt(s)
				else if String.substring(s,0,1)="~" then isPosInt(String.substring(s,1,size(s)-1))
				else isPosInt(s);
 
fun readln () = ( case TextIO.inputLine(TextIO.stdIn) of
	 			SOME s => chomp1(s)
       		| 	NONE =>  ""
    	);

fun machine (output:string,ram:(string*string*string*string) list,pc:int,sym_table:(string*string*string)list,n:string) = 
			let 
				val (a,b,c,d) = getnth(ram,pc)
			in
				case (a) of
					"DECLARE_INT"	=>	((*print (a^" "^b^" "^c^" "^d^"\n");
										printsymtable(sym_table);print "\n";*)
										if (c="_") then
											machine(output,ram,pc+1,(b,"null","INT")::sym_table,n)
										else
											machine(output,ram,pc+1,(b,c,"INT")::sym_table,n))	
				
				|	"DECLARE_BOOL"	=>	((*print (a^" "^b^" "^c^" "^d^"\n");
										printsymtable(sym_table);print "\n";*)
										if (c="_") then
											machine(output,ram,pc+1,(b,"null","BOOL")::sym_table,n)
										else
											machine(output,ram,pc+1,(b,c,"BOOL")::sym_table,n))
				
				|	"DECLARE_PROC"	=>	((*print (a^" "^b^" "^c^" "^d^"\n");
										printsymtable(sym_table);print "\n";*)
										if (c="_") then
											machine(output,ram,pc+1,(b,"null","PROC")::sym_table,n)
										else
											machine(output,ram,pc+1,(b,c,"PROC")::sym_table,n))
				
				|	"PRINT"			=>	((*print (a^" "^b^" "^c^" "^d^"\n");
										printsymtable(sym_table);print "\n";*)
										printStringtoFile(get(sym_table,b)^"\n",output);machine(output,ram,pc+1,sym_table,n))

				|	"READ"			=>	((*print (a^" "^b^" "^c^" "^d^"\n");
										printsymtable(sym_table);print "\n";*)
										let
											val x = readln()
											val typeofd = gettype(sym_table,d)
										in	
											if typeofd = "BOOL" then
												if x = "tt" orelse x="ff" then machine(output,ram,pc+1,put(sym_table,(d,x,"BOOL"),[]),n)
												else raise Failure(x^" is not of type BOOL")
											else if typeofd = "INT" then
												if isInt(x) then machine(output,ram,pc+1,put(sym_table,(d,x,"INT"),[]),n)
												else raise Failure(x^" is not of type INT")
											else raise Failure("Invalid type: "^x^" not of type INT or BOOL")		
   										end)
				
				|	"CALL"			=>	((*print (a^" "^b^" "^c^" "^d^"\n");
										printsymtable(sym_table);print "\n";*)
										machine(output,ram,trunc(Option.getOpt(Real.fromString(get(sym_table,b)),0.0)),("proc call",Int.toString(pc),"")::sym_table,n))
				
				|	"IF"			=>	((*print (a^" "^b^" "^c^" "^d^"\n");
										printsymtable(sym_table);print "\n";*)
										if (get(sym_table,b)="tt") then machine(output,ram,pc+1,sym_table,n)
										else machine(output,ram,trunc(Option.getOpt(Real.fromString(c),0.0)),sym_table,n))

				|	"GOTO"			=> ((*print (a^" "^b^" "^c^" "^d^"\n");
										printsymtable(sym_table);print "\n";*)
										machine(output,ram,trunc(Option.getOpt(Real.fromString(c),0.0)),sym_table,n))
				
				|	"RETURN"		=> ((*print (a^" "^b^" "^c^" "^d^"\n");
										printsymtable(sym_table);print "\n";*)
										case sym_table of
										[] 		=> machine(output,ram,pc,sym_table,n)
									|	x::xs	=> if ((#1 x)="proc call") then machine(output,ram,trunc(Option.getOpt(Real.fromString((#2 x)),0.0))+1,xs,n)
													else machine(output,ram,pc,xs,n))
				
				|	"ASSIGN"		=>	((*print (a^" "^b^" "^c^" "^d^"\n");
										printsymtable(sym_table);print "\n";*)
										if b = "tt" orelse b="ff" then  machine(output,ram,pc+1,put(sym_table,(d,b,"BOOL"),[]),n)
										else if isInt(b) then machine(output,ram,pc+1,put(sym_table,(d,b,"INT"),[]),n)
										else if get(sym_table,b)="Not found Exception raised" then raise Failure("Value not found")
										else machine(output,ram,pc+1,put(sym_table,(d,get(sym_table,b),gettype(sym_table,b)),[]),n))
				
				|	"PLUS"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff" orelse get(sym_table,c)="tt" orelse get(sym_table,c)="ff") then
								raise Failure("Wrong type")
							else
							let
								val v = bi2str(add(str2bi(get(sym_table,b)),str2bi(get(sym_table,c))))
							in
								machine(output,ram,pc+1,put(sym_table,(d,v,"INT"),[]),n)
							end	)
				
				|	"MINUS"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff" orelse get(sym_table,c)="tt" orelse get(sym_table,c)="ff") then
								raise Failure("Wrong type")
							else
							let
								val v = bi2str(sub(str2bi(get(sym_table,b)),str2bi(get(sym_table,c))))
							in
								machine(output,ram,pc+1,put(sym_table,(d,v,"INT"),[]),n)
							end)
				
				|	"MULT"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff" orelse get(sym_table,c)="tt" orelse get(sym_table,c)="ff") then
								raise Failure("Wrong type")
							else
							let
								val v = bi2str(mul(str2bi(get(sym_table,b)),str2bi(get(sym_table,c))))
							in
								machine(output,ram,pc+1,put(sym_table,(d,v,"INT"),[]),n)
							end)
				
				|	"DIV"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff" orelse get(sym_table,c)="tt" orelse get(sym_table,c)="ff") then
								raise Failure("Wrong type")
							else
							let
								val v = bi2str(div4bigint(str2bi(get(sym_table,b)),str2bi(get(sym_table,c))))
							in
								machine(output,ram,pc+1,put(sym_table,(d,v,"INT"),[]),n)
							end)

				|	"MOD"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff" orelse get(sym_table,c)="tt" orelse get(sym_table,c)="ff") then
								raise Failure("Wrong type")
							else
							let
								val v = bi2str(mod4bigint(str2bi(get(sym_table,b)),str2bi(get(sym_table,c))))
							in
								machine(output,ram,pc+1,put(sym_table,(d,v,"INT"),[]),n)
							end)
				|	"GEQ"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff" orelse get(sym_table,c)="tt" orelse get(sym_table,c)="ff") then 
										raise Failure("Invalid types")
							else
							let
								val v = (geq(str2bi(get(sym_table,b)),str2bi(get(sym_table,c))))
							in
								(if v = true then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
								else machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n))
							end)
				|	"GT"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff" orelse get(sym_table,c)="tt" orelse get(sym_table,c)="ff") then 
										raise Failure("Invalid types")
							else
							let
								val v = (gt(str2bi(get(sym_table,b)),str2bi(get(sym_table,c))))
							in
								(if v = true then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
								else machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n))
							end)
				|	"LEQ"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff" orelse get(sym_table,c)="tt" orelse get(sym_table,c)="ff") then 
										raise Failure("Invalid types")
							else
							let
								val v = (leq(str2bi(get(sym_table,b)),str2bi(get(sym_table,c))))
							in
								(if v = true then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
								else machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n))
							end)
				|	"LT"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff" orelse get(sym_table,c)="tt" orelse get(sym_table,c)="ff") then 
										raise Failure("Invalid types")
							else
							let
								val v = (lt(str2bi(get(sym_table,b)),str2bi(get(sym_table,c))))
							in
								(if v = true then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
								else machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n))
							end)
				|	"NEQ"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff" orelse get(sym_table,c)="tt" orelse get(sym_table,c)="ff") then 
										raise Failure("Invalid types")
							else
							let
								val v = (neq(str2bi(get(sym_table,b)),str2bi(get(sym_table,c))))
							in
								(if v = true then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
								else machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n))
							end)
				|	"EQ"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff" orelse get(sym_table,c)="tt" orelse get(sym_table,c)="ff") then 
										raise Failure("Invalid types")
							else
							let
								val v = (eq(str2bi(get(sym_table,b)),str2bi(get(sym_table,c))))
							in
								(if v = true then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
								else machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n))
							end)
				|	"AND"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else raise Failure("Invalid types"))
							
				|	"OR"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="tt" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="tt" ) then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else if (get(sym_table,b)="ff" andalso get(sym_table,b)="ff" ) then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else raise Failure("Invalid types"))
							
				|	"UPLUS"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff") then raise Failure("Wrong type")
							else
							let
								val v = (get(sym_table,b))
							in
								machine(output,ram,pc+1,put(sym_table,(d,v,"INT"),[]),n)
							end)
				|	"UMINUS"=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt" orelse get(sym_table,b)="ff") then raise Failure("Wrong type")
							else
							let
								val v = bi2str(unminus(str2bi(get(sym_table,b))))
							in
								machine(output,ram,pc+1,put(sym_table,(d,v,"INT"),[]),n)
							end)
				|	"NOT"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
								printsymtable(sym_table);print "\n";*)
							if (get(sym_table,b)="tt") then machine(output,ram,pc+1,put(sym_table,(d,"ff","BOOL"),[]),n)
							else if (get(sym_table,b)="ff") then machine(output,ram,pc+1,put(sym_table,(d,"tt","BOOL"),[]),n)
							else raise Failure("Invalid type"))

				|	"END_OF_CODE"	=> ((*print (a^" "^b^" "^c^" "^d^"\n");
										printsymtable(sym_table);print "\n";*)
										printStringtoFile("",output))
			end;

fun execute (input,output,m) = machine(output,listoftuples(readfile(input),[]),0,[],m); 
(*readfile("input.txt");*)
execute(out_code,out_value,"0");
(*execute("input.txt","output.txt","10");*)

OS.Process.exit(OS.Process.success);