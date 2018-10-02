signature BigInt =
sig
type bigint
val getbigint: int -> bigint
val bi2str : bigint -> string
val str2bi : string -> bigint
val lt : bigint * bigint -> bool
val leq : bigint * bigint -> bool
val gt : bigint * bigint -> bool
val geq : bigint * bigint -> bool
val eq : bigint * bigint -> bool
val neq : bigint * bigint -> bool
val div4bigint : bigint * bigint -> bigint
val mul : bigint * bigint -> bigint
val add : bigint * bigint -> bigint
val sub : bigint * bigint -> bigint
val mod4bigint : bigint * bigint -> bigint
val unminus : bigint -> bigint
end;

structure BigInt:BigInt =
	struct

				type bigint = string;

				exception DivByZeroException;

				fun getbigint(a) =
					Int.toString(a)

				and bi2str(a) =
					a

				and str2bi(a) = 
					a

				and remz([]) = [] |
					remz (cur::rem) =
						if(Char.compare(cur,#"0") = EQUAL) then
							remz(rem)
						else
							cur::rem

				and retmod([]) = [] |
					retmod(cur::rem) =
					if(Char.compare(cur,#"~") = EQUAL) then
						rem
					else
						cur::rem

				and isneg([]) =false|
					isneg(cur::rem) =
					if(Char.compare(cur,#"~") = EQUAL) then
						true
					else
						false

				and 	cm_i(nil,nil,curstat) =
							curstat
					|	cm_i(nil,curb::restb, curstat) =
							2
					| 	cm_i(cura::resta,nil, curstat) =
							0
					|	cm_i(cura::resta, curb::restb, curstat) =
						if( Char.compare(cura,curb) = EQUAL) then
							cm_i( resta, restb, curstat)
						else if( Char.compare(cura,curb) = GREATER) then
							cm_i( resta, restb, 0)
						else
							cm_i( resta, restb, 2)

				(*Csae where a b have diff lengths*)
				(*Both are strings*)

				and gt(a,b) =
					let val x = cm_i( rev(remz(retmod(String.explode(a)))), rev(remz(retmod(String.explode(b)))), 1 );
						val aneg = isneg(String.explode(a));
						val bneg = isneg(String.explode(b))
					in 	if(x = 0) then
							if aneg = false andalso bneg = false then
								true
							else if aneg = true andalso bneg = false then
								false
							else if aneg = false andalso bneg = true then
								true
							else (*if aneg = true andalso bneg = true then*)
								false
						else
							if aneg = false andalso bneg = false then
								false
							else if aneg = true andalso bneg = false then
								false
							else if aneg = false andalso bneg = true then
								true
							else (*if aneg = true andalso bneg = true then*)
								if x = 1 then
									false
								else
									true
					end

				and geq(a,b) =
					let val x = cm_i( rev(remz(retmod(String.explode(a)))), rev(remz(retmod(String.explode(b)))), 1 );
						val aneg = isneg(String.explode(a));
						val bneg = isneg(String.explode(b))
					in 	if x = 0 orelse x = 1 then
							if aneg = false andalso bneg = false then
								true
							else if aneg = true andalso bneg = false then
								false
							else if aneg = false andalso bneg = true then
								true
							else (*if aneg = true andalso bneg = true then*)
								if x = 1 then
									true
								else
									false
						else
							if aneg = false andalso bneg = false then
								false
							else if aneg = true andalso bneg = false then
								false
							else if aneg = false andalso bneg = true then
								true
							else (*if aneg = true andalso bneg = true then*)
								true
								
					end

				and lt(a,b) =
					not (geq(a,b))

				and leq(a,b) =
					not (gt(a,b))
					
				and neq(a,b) =
					not (eq(a,b))

				and modadd(nil,nil, carry) = 
					if carry = 0 then
						nil
					else
						Char.chr( carry + 48)::nil
					|modadd(cura::resta, nil, carry) =
					let val ta = Int.fromString(String.implode(cura::nil));
						val diga = getOpt(ta,0);
						val newc = (diga + carry) div 10; 
						val newd = Char.chr(((diga + carry) mod 10) + 48)
					in newd::modadd( resta, nil, newc )
					end
					|modadd( nil, curb::restb, carry) =
					let val tb = Int.fromString(String.implode(curb::nil));
						val digb = getOpt(tb,0);
						val newc = (digb + carry) div 10; 
						val newd = Char.chr(((digb + carry) mod 10) + 48)
					in newd::modadd( nil, restb, newc )
					end
					|modadd(cura::resta,curb::restb,carry) =
					let val ta = Int.fromString(String.implode(cura::nil));
						val diga = getOpt(ta,0);
						val tb = Int.fromString(String.implode(curb::nil));
						val digb = getOpt(tb,0);
						val newc = (diga + digb + carry) div 10; 
						val newd = Char.chr(((diga + digb + carry) mod 10) + 48)
					in newd::modadd( resta, restb, newc )
					end


				(*REMOVE LEADING ZEROS*)
				(*ENSURE A>B*)
				and modsub(nil,nil, carry) = 
					if carry = 0 then
						nil
					else
						Char.chr( carry + 48)::nil
					|modsub(cura::resta, nil, carry) =
					let val ta = Int.fromString(String.implode(cura::nil));
						val diga = getOpt(ta,0);
						val newc = (diga + carry) div 10; 
						val newd = Char.chr(((diga + carry) mod 10) + 48)
					in newd::modsub( resta, nil, newc )
					end
					|modsub( nil, curb::restb, carry) =
					let val tb = Int.fromString(String.implode(curb::nil));
						val digb = getOpt(tb,0);
						val newc = (digb + carry) div 10; 
						val newd = Char.chr(((0 - digb + carry) mod 10) + 48)
					in newd::modsub( nil, restb, newc )
					end
					|modsub(cura::resta,curb::restb,carry) =
					let val ta = Int.fromString(String.implode(cura::nil));
						val diga = getOpt(ta,0);
						val tb = Int.fromString(String.implode(curb::nil));
						val digb = getOpt(tb,0);
						val newc = (diga - digb + carry) div 10; 
						val newd = Char.chr(((diga - digb + carry) mod 10) + 48)
					in newd::modsub( resta, restb, newc )
					end

				and addt(a,b) =
					let val moda = remz(retmod(String.explode(a)));
						val modb = remz(retmod(String.explode(b)));
						val nega = isneg(String.explode(a));
						val negb = isneg(String.explode(b));
						val inpa = rev(moda);
						val inpb = rev(modb);
					in 	if nega = negb then
							if nega = true then
								("~" ^ String.implode(rev(modadd(inpa,inpb,0))))
							else
								String.implode(rev(modadd(inpa,inpb,0)))
						else
							if nega = true then (*A -ve B +ve*)
								if gt(String.implode(moda),String.implode(modb)) = true then
									("~" ^ String.implode(rev(modsub(inpa,inpb,0))))
								else if lt(String.implode(moda),String.implode(modb)) = true then
									(String.implode(rev(modsub(inpb,inpa,0))))
								else
									""
							else				(*A +ve B -ve*)
								if gt(String.implode(moda),String.implode(modb)) = true then
									(String.implode(rev(modsub(inpa,inpb,0))))
								else if lt(String.implode(moda),String.implode(modb)) = true then
									("~" ^ String.implode(rev(modsub(inpb,inpa,0))))
								else
									""
					end

				and add(a,b) =
					let val x = addt(a,b);
					in 	if x = "" then
							"0"
						else 
							if lt(x,"0") = true then
								"~"^String.implode(remz(String.explode(String.substring(x,1,String.size(x)-1))))
							else
								String.implode(remz(String.explode(x)))
					end

				and eq(a,b) =
					let val x = cm_i( rev(remz(retmod(String.explode(a)))), rev(remz(retmod(String.explode(b)))), 1 );
						val aneg = isneg(String.explode(add(a,"0")));
						val bneg = isneg(String.explode(add(b,"0")))
					in 	if x = 1 then
							if aneg = bneg then
								true
							else
								false
						else
							false				
					end

				and sub(a,b) =
					if(String.compare (String.substring(b,0,1) , "~") = EQUAL ) then
						add(a,String.substring(b,1,String.size(b)-1))
					else
						add(a,("~" ^ b) )


				and modmul( nil, curb, carry) =
					if carry = 0 then
						nil
					else
						Char.chr( carry + 48)::nil
					|modmul(cura::resta,curb,carry) =
					let val ta = Int.fromString(String.implode(cura::nil));
						val diga = getOpt(ta,0);
						val tb = Int.fromString(String.implode(curb::nil));
						val digb = getOpt(tb,0);
						val newc = (diga * digb + carry) div 10; 
						val newd = Char.chr(((diga * digb + carry) mod 10) + 48)
					in newd::modmul( resta, curb, newc )
					end


				and mult(a,"",c) =
					add(c,String.implode(modmul(a,#"0",0)))
					| mult(a,b,c) =
					let val curmuldig = hd(String.explode(b));
						val remb = String.implode(tl(String.explode(b)));

					in  mult(a,remb, add(c^"0",String.implode(rev(modmul(a,curmuldig,0)))  ) )
					end
					
				and mul(a,b) =
					let val reva = rev(String.explode(a));
						val nega = lt(add(a,"0"),"0");
						val negb = lt(add(b,"0"),"0")
					in
						if(nega = negb) then
							mult(retmod(reva),String.implode(retmod(String.explode(b))),"0")
						else
							"~"^mult(retmod(reva),String.implode(retmod(String.explode(b))),"0")
					end


				and div_incb(a,b) =
					if(gt(a,b) = true) then
						b::div_incb( a,mul(b,"2") )
					else
						b::nil

				and reduce(b,bin) =
					if lt(hd(b),bin) then
						nil
					else
						tl(b)

				and div_red(a,nil,c,bin)=
						c
				|	div_red(a,b,c, bin)=
				(*	(print("a:" ^ a ^ "b:" ^ hd(b) ^ "c:" ^ c ^ "bin:" ^ bin ^ "\n");
				*)	if(geq(a,hd(b)) = true) then
						div_red(sub(a,hd(b)), reduce(b,bin), add(mul(c,"2"),"1"), bin)
					else
						div_red(a, reduce(b,bin), mul(c,"2"), bin)
						

				and div1(a,b) =
					let val barr = div_incb(a,b);
						val quot = div_red(a,rev(barr),"0",b)
					in quot
					end

				and div4bigint(a,b) =
					let val nega = lt(add(a,"0"),"0");
						val negb = lt(add(b,"0"),"0");
						val moda = String.implode(retmod(String.explode(add(a,"0"))));
						val modb = String.implode(retmod(String.explode(add(b,"0"))));
						val quo =  if(eq(b,"0") = false) then
									div1(moda,modb)
								   else
								   	"0";
						val negquo = ("~"^quo)
						in 
							if eq(b,"0") = false then
								if(nega = negb) then
									quo
								else
									if( eq( mul(negquo,b),a) = false ) then
										sub(negquo,"1")
									else
										negquo
							else
								raise DivByZeroException
					end

				and unminus(a) =
					if lt(a,"0") = true then
						String.substring(a,1,String.size(a)-1)
					else
						"~"^a

				and mod4bigint(a,b) =
					let val nega = lt(add(a,"0"),"0");
						val negb = lt(add(b,"0"),"0");
						val moda = String.implode(retmod(String.explode(add(a,"0"))));
						val modb = String.implode(retmod(String.explode(add(b,"0"))));
						val quo =  if(eq(b,"0") = false) then
									div1(moda,modb)
								   else
								   	"0";
						val negquo = ("~"^quo)
						in 	
							if eq(b,"0") = false then
								if(nega = negb) then
									sub(a,mul(quo,b))
								else
									if( eq( mul(negquo,b),a) = false ) then
										add(sub(a,mul(negquo,b)),b)
									else
										sub(a,mul(negquo,b))

							else
								raise DivByZeroException
					end;

end;
open BigInt;

div4bigint("0","23");
div4bigint("~0","23");

div4bigint("123","23");
div4bigint("~123","23");
div4bigint("~123","~23");
div4bigint("123","~23");

mod4bigint("123","23");
mod4bigint("~123","23");
mod4bigint("~123","~23");
mod4bigint("123","~23");

add("00","~000");
add("1000","001000");
add("1000","~002000");
add("~1000","~2000");
add("~1000","2000");

sub("2","34");
sub("1000","1000");
sub("1000","001000");
sub("1000","~002000");
sub("~1000","~2000");
sub("~1000","2000");
(*
mul("321","423");
mul("~0","234");
mul("~12","24");
mul("324","~999");


gt("0","2");
gt("23","242");
gt("~23","242");
gt("200","00100");
gt("~200","00100");
gt("99","111");
neq("2","02");
neq("2","3");
gt("00002","34");
gt("234","234");
geq("234","00234");
gt("234","432");
gt("2","0");
gt("","2");
gt("242","23");
gt("111","89");*)