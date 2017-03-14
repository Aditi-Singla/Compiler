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
    val unminus : bigint -> bigint;
end

structure BigInt:BigInt =
  struct

    type bigint = int * int list;
    
    exception DivByZeroException;
    
    fun divide(x) = if (x=0) then [] else (x mod 10:: (divide(x div 10)));

    (*Input-integer ; Output-Bigint*)
    fun getbigint (x) :bigint = 
        if x=0 then (0,[0])
         else if x>0 then ((0,divide(x)):bigint) 
         else ((1,divide(~x)):bigint);

    fun clearZerolist (l) = if l = [] then [0]
                  else if (hd (rev l))=0 then clearZerolist(rev (tl (rev l)))
                          else l;  
     
    fun listtostr (l) = case l of
        []  => ""
      | (x::xs) => listtostr(xs)^Int.toString(x);
    
    fun bi2str (b:bigint) = case b of
        (0,l) => listtostr(clearZerolist l)   
      | (1,l) => "~"^listtostr(clearZerolist l)
      | _ => "";

    fun clearZerostring s = if s = "" then ""
                            else if String.substring(s,0,1) = "~" then "~"^clearZerostring(String.substring(s,1,size(s)-1))
                            else if s = "0" then ""
                            else if String.substring(s,0,1) = "0" then clearZerostring(String.substring(s,1,size(s)-1))
                            else s;

    fun strtolist (s) = if s = "" then [] 
            else strtolist(String.substring(s,1,size(s)-1))@trunc(Option.getOpt(Real.fromString(String.substring(s,0,1)),0.0))::[];
    
    fun str2bi (str) :bigint = if (clearZerostring str = "") orelse (clearZerostring str = "~") then (0,[0])
                else if String.substring(str,0,1) = "~" then (1,strtolist(String.substring(str,1,size(str)-1)))
                else (0,strtolist(clearZerostring str));

    fun checksame ([],[]) = true
      | checksame (x::xs,[]) = false
      | checksame ([],y::ys) = false
      | checksame (x::xs,y::ys) = ((x=y) andalso (checksame(xs,ys)));

    fun checkless (a,b) = if (length(a) < length(b)) then true
                    else if (length(a) = length(b)) then 
                          (if length a = 0 then false 
                          else if (hd a >= hd b) then checkless(tl a,tl b)
                              else (checksame (tl a, tl b) orelse checkless(tl a, tl b)))
                    else false;

    fun addl ([],[],c) = if c = 0 then [] else [c]
      | addl (x::xs,[],c) = let 
                              val s = (x+c) mod 10
                              val carry = (x+c) div 10
                            in
                              s::addl(xs,[],carry)
                            end
                              
      | addl ([],y::ys,c) = addl(y::ys,[],c)
      | addl (x::xs,y::ys,c) = let 
                                val s = (x+y+c) mod 10
                                val carry = (x+y+c) div 10
                              in
                                s::addl(xs,ys,carry)
                              end  

    (*a>b*)
    and subl ([],[],c) = if c = 0 then [] else [c]
      | subl (x::xs,[],c) = addl(x::xs,[],c)
      | subl ([],y::ys,c) = (*junk*)
                            let 
                              val s = (c-y) mod 10
                              val carry = (c+y) div 10
                            in
                              s::subl(ys,[],carry)
                            end  
      | subl (x::xs,y::ys,c) = let 
                              val s = (x+c-y) mod 10
                              val carry = (x+c-y) div 10
                            in
                              s::subl(xs,ys,carry)                       
                            end

    and mult(a,[],c) = [c]
    |   mult(a,x::xs,c) = let 
                            val s = ((a*x)+c) mod 10
                            val carry = ((a*x)+c) div 10
                          in
                            s::mult(a,xs,carry)
                          end

    and addmul([],l:int list,acc,ans) = ans
      | addmul(x::xs,l:int list,acc,ans) = addmul(xs,l,0::acc,addl((acc@mult(x,l,0)),ans,0))

    and addzeroes(l,n) = if n<=0 then l
                        else addzeroes(0::l,n-1)

    and equalise(a,b) = addzeroes(a,length(b)-length(a)-1) 

    and findfactor(l1,l2,m) = if gt ((0,addmul(l1,[m+1],[],[])),(0,l2)) then (m,addmul(l1,[m],[],[]))
                              else findfactor(l1,l2,m+1) 

    and findmaxless([],[]) = (0,[])
      | findmaxless(x::xs,y::ys) = findfactor(x::xs,y::ys,0)

    (*l2/l1*)
    and divide(l1,l2,q) = if clearZerolist(l1) = [0] then raise DivByZeroException
                        else if gt((0,l1),(0,l2)) then (q,l2)
                        else 
                          let
                            val (s,t) = findmaxless(equalise(l1,l2),l2)
                            val u = subl(l2,t,0)
                          in
                            divide(equalise(l1,u),u,s::q)
                          end

    and eq (a,b) = case (a,b) of
        ((0,l1),(0,l2)) => checksame (clearZerolist l1,clearZerolist l2)
      | ((0,l1),(1,l2)) => false
      | ((1,l1),(0,l2)) => false
      | ((1,l1),(1,l2)) => checksame (clearZerolist l1,clearZerolist l2)
      | (_,_) => false

    and lt (a,b) = case (a,b) of
        ((0,l1),(0,l2)) => checkless (clearZerolist l1,clearZerolist l2)
      | ((0,l1),(1,l2)) => false
      | ((1,l1),(0,l2)) => true
      | ((1,l1),(1,l2)) => not (checkless (clearZerolist l1,clearZerolist l2)) andalso not (checksame (clearZerolist l1,clearZerolist l2))
      | (_,_) => false

    and leq(a,b) = lt(a,b) orelse eq(a,b)
    
    and gt (a,b) = not (leq(a,b))

    and geq(a,b) = not (lt(a,b))

    and neq(a,b) = not (eq(a,b))  

    and add (a:bigint,b:bigint) :bigint = case (a,b) of
        ((0,l1),(0,l2)) => (0,addl(l1,l2,0))
      | ((0,l1),(1,l2)) => if (geq((0,l1),(0,l2))) then (0,subl(l1,l2,0))
                          else (1,subl(l2,l1,0))
      | ((1,l1),(0,l2)) => if (geq((0,l2),(0,l1))) then (0,subl(l2,l1,0))
                          else (1,subl(l1,l2,0))
      | ((1,l1),(1,l2)) => (1,addl(l1,l2,0))
      | (_,_)   => (0,[0])

    and sub (a:bigint,b:bigint) :bigint = case (a,b) of
        ((0,l1),(0,l2)) =>  if (geq(a,b)) then (0,subl(l1,l2,0))
                          else (1,subl(l2,l1,0))
      | ((0,l1),(1,l2)) => (0,addl(l1,l2,0))
      | ((1,l1),(0,l2)) => (1,addl(l1,l2,0))
      | ((1,l1),(1,l2)) => if (geq((0,l2),(0,l1))) then (0,subl(l2,l1,0))
                          else (1,subl(l1,l2,0))
      | (_,_)   => (0,[0])

    and mul (a:bigint,b:bigint) :bigint = case (a,b) of
        ((0,l1),(0,l2)) => (0,addmul(l1,l2,[],[]))
      | ((0,l1),(1,l2)) => (1,addmul(l1,l2,[],[]))
      | ((1,l1),(0,l2)) => (1,addmul(l1,l2,[],[]))
      | ((1,l1),(1,l2)) => (0,addmul(l1,l2,[],[]))
      | (_,_)   => (0,[0])  

    and div4bigint (a:bigint,b:bigint) :bigint = case (a,b) of
        ((0,l1),(0,l2)) => let
                            val (q,r) = divide(l2,l1,[0])
                           in
                            (0,q)
                           end 
      | ((0,l1),(1,l2)) => let
                            val (q,r) = divide(l2,l1,[0])
                           in
                              if clearZerolist(r)=[0] then (1,q)
                              else (1,addl(q,[1],0))
                           end
      | ((1,l1),(0,l2)) => let
                            val (q,r) = divide(l2,l1,[0])
                           in
                              if clearZerolist(r)=[0] then (1,q)
                              else (1,addl(q,[1],0))
                           end
      | ((1,l1),(1,l2)) => let
                            val (q,r) = divide(l2,l1,[0])
                           in
                            (0,q)
                           end
      | (_,_)   => (0,[0])

    and mod4bigint (a:bigint,b:bigint) :bigint = case (a,b) of
        ((0,l1),(0,l2)) => let
                            val (q,r) = divide(l2,l1,[0])
                           in
                            (0,r)
                           end 
      | ((0,l1),(1,l2)) => let
                            val (q,r) = divide(l2,l1,[0])
                           in
                              if clearZerolist(r)=[0] then (0,r)
                              else (1,subl(l2,r,0))
                           end
      | ((1,l1),(0,l2)) => let
                            val (q,r) = divide(l2,l1,[0])
                           in
                              if clearZerolist(r)=[0] then (0,r)
                              else (0,subl(l2,r,0))
                           end
      | ((1,l1),(1,l2)) => let
                            val (q,r) = divide(l2,l1,[0])
                           in
                              if clearZerolist(r)=[0] then (0,r)
                              else (1,r)
                           end
      | (_,_)   => (0,[0])

    and unminus(b) : bigint = case b of
            (0,l) => if (clearZerolist l) = [0] then (0,[0]) else (1,l)
          | (1,l) => (0,l)
          | _   =>  (0,[0]);


end;
open BigInt;
(*getbigint(24);
getbigint(0);
getbigint(20274849);
getbigint(~10);
bi2str((0,[]));
bi2str((0,[0,0,0]):bigint);
bi2str((1,[0]));
bi2str((0,[1,2,3,5,6,0,0,0,0]));
bi2str((1,[1,2,3,5,6]));
str2bi("00001234");
str2bi("~1234");
str2bi("123464729");
str2bi("0");
str2bi("0000");
str2bi("1");
unminus((0,[]));
unminus((0,[0]));
unminus((0,[0,0,0,0]));
unminus((1,[0]));
unminus((0,[1,2,3,5,6]));
unminus((1,[1,2,3,5,6]));

gt(str2bi "0",str2bi "2");
gt(str2bi "23",str2bi "242");
gt(str2bi "~23",str2bi "242");
gt(str2bi "200",str2bi "00100");
gt(str2bi "~200",str2bi "00100");
gt(str2bi "99",str2bi "111");
neq(str2bi "2",str2bi "02");
neq(str2bi "2",str2bi "3");
gt(str2bi "00002",str2bi "34");
gt(str2bi "234",str2bi "234");
geq(str2bi "234",str2bi "00234");
gt(str2bi "234",str2bi "432");
gt(str2bi "2",str2bi "0");
gt(str2bi "",str2bi "2");
gt(str2bi "242",str2bi "23");
gt(str2bi "111",str2bi "89");

bi2str(add(str2bi "00",str2bi "~000"));
bi2str(add(str2bi "1000",str2bi "001000"));
bi2str(add(str2bi "1000",str2bi "~002000"));
bi2str(add(str2bi "~1000",str2bi "~2000"));
bi2str(add(str2bi "~1000",str2bi "2000"));

bi2str(sub(str2bi "2",str2bi "34"));
bi2str(sub(str2bi "1000",str2bi "1000"));
bi2str(sub(str2bi "1000",str2bi "001000"));
bi2str(sub(str2bi "1000",str2bi "~002000"));
bi2str(sub(str2bi "~1000",str2bi "~2000"));
bi2str(sub(str2bi "~1000",str2bi "2000"));

bi2str(mul(str2bi "321",str2bi "423"));
bi2str(mul(str2bi "~0",str2bi "234"));
bi2str(mul(str2bi "~12",str2bi "24"));
bi2str(mul(str2bi "324",str2bi "~999"));

bi2str(div4bigint(str2bi "0",str2bi "23"));
bi2str(div4bigint(str2bi "~0",str2bi "23"));

bi2str(div4bigint(str2bi "123",str2bi "23"));
bi2str(div4bigint(str2bi "~123",str2bi "23"));
bi2str(div4bigint(str2bi "~123",str2bi "~23"));
bi2str(div4bigint(str2bi "123",str2bi "~23"));
bi2str(div4bigint(str2bi "48",str2bi "3"));
bi2str(div4bigint(str2bi "48",str2bi "~3"));
bi2str(div4bigint(str2bi "~48",str2bi "3"));
bi2str(div4bigint(str2bi "~48",str2bi "~3"));

bi2str(mod4bigint(str2bi "123",str2bi "23"));
bi2str(mod4bigint(str2bi "~123",str2bi "23"));
bi2str(mod4bigint(str2bi "~123",str2bi "~23"));
bi2str(mod4bigint(str2bi "123",str2bi "~23"));
bi2str(mod4bigint(str2bi "48",str2bi "3"));
bi2str(mod4bigint(str2bi "48",str2bi "~3"));
bi2str(mod4bigint(str2bi "~48",str2bi "3"));
bi2str(mod4bigint(str2bi "~48",str2bi "~3"));
*)