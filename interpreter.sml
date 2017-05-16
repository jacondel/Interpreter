datatype token = INT of int 
  | BOOL of bool
  | OP of string
  | NAME of string
  | STRING of string
  | ERROR
  | QUIT
  | UNIT
  | FUN of (string) * (string) * (token list) * ((token*token)list) * string
  | FUNEND of (string*string)
  | RETURN of (string*string)


fun split "" d acc = (implode acc)::[]
  | split str d acc = 
  let val c1 = hd (explode str)
      val rest = implode (tl (explode str))
  in
    if (c1=d) 
    then (implode acc):: (split rest d [])
    else split rest d (acc @ [c1])
  end

fun dropLast (x::[]) = []
  | dropLast (x::xs) = x::(dropLast xs) 


fun isName s =
    let 
      fun isNum c = (ord c > 47) andalso (ord c < 58)
      fun isChar c = ((ord c > 64) andalso (ord c < 91))
        orelse ((ord c > 96) andalso (ord c < 123));
      fun areAlphaNumeric [] = true
        | areAlphaNumeric (c::[]) = (isChar(c) orelse isNum(c))
        | areAlphaNumeric (c::cs) = (isChar(c) orelse isNum(c)) andalso areAlphaNumeric(cs)
    in
        isChar(hd(explode(s))) andalso areAlphaNumeric(tl(explode(s)))
    end

fun isBool s = (s = ":true:") orelse (s = ":false:")
fun isQuit s = (s = "quit")
fun isError s = (s = ":error:")
fun isReturn s = (s = "return")
fun isFunEnd s = (s = "funEnd")

fun isOperator s = 
    let 
      val ops = ["add","sub","mul","div","rem","neg","swap","pop","and","or","not","equal","lessThan","bind","if","let","end","call"]
    in 
      List. exists (fn x => x = s) ops
    end

fun isString s =
    let 
      val first = hd(explode(s))
      val last = List.last(explode(s))
    in
      case (first,last) of 
        (#"\"",#"\"") => true
        | _           => false
    end

fun isInt sn =
    let 
      fun isNum c = (ord c > 47) andalso (ord c < 58)
      fun areNum [] = false
        | areNum (x::[]) = isNum(x)
        | areNum (x::xs) = isNum(x) andalso areNum(xs)
      val s = if ((hd(explode sn))= #"-") then implode(List.drop ((explode sn),1)) else sn
    in
        areNum(explode(s))
    end

fun isFun s = (length(explode(s)) > 4) andalso (substring(s,0,4) = "fun ")
fun isIOFun s = (length(explode(s)) > 9) andalso (substring(s,0,9) = "inOutFun ")

fun asInt s = valOf (Int.fromString(s))
fun asBool s = if (s=":true:") then true else false
fun asFun s t = 
  let 
    val [_,name,arg] = split s #" " []
  in
    (name,arg,[],[],t)
  end

fun tokenize sp = 
    let 
      val s = if (length(explode(sp)) > 5) andalso (substring(sp,0,5) = "push ") then implode(List.drop((explode sp),5)) else sp
    in
      if (isOperator(s)) then OP s
      else if (isFun(s)) then FUN( asFun s "")
      else if (isIOFun(s)) then FUN( asFun s "io")
      else if (isFunEnd(s)) then FUNEND("","")
      else if (isReturn(s)) then RETURN("","")
      else if (isInt(s)) then INT (asInt s)
      else if (isBool(s)) then BOOL (asBool s)
      else if (isQuit(s)) then QUIT
      else if (isName(s)) then NAME s
      else if (isString(s)) then STRING (implode(List.take ((tl(explode s)) , length(explode(s))-2)))
      else ERROR
    end

fun compress [] outtk funInfo ig =  outtk
  | compress ((FUN(name,param,commands,env,typ))::tks) outtk funInfo ig = compress tks outtk ((FUN(name,param,commands,env,typ))::funInfo) ig
  | compress (FUNEND("","")::tks) outtk (FUN(name1,param1,commands1,env1,typ1)::(FUN(name,param,commands,env,typ))::fs) false =  compress tks outtk ((FUN(name,param,commands@[FUN(name1,param1,commands1@[FUNEND("","")],env1,typ1)],env,typ))::fs) false
  | compress (FUNEND("","")::tks) outtk (FUN(name1,param1,commands1,env1,typ1)::fs) false =  compress tks (outtk@[FUN(name1,param1,commands1@[FUNEND("","")],env1,typ1)]) fs false 
  | compress (FUNEND("","")::tks) outtk (FUN(name1,param1,commands1,env1,typ1)::(FUN(name,param,commands,env,typ))::fs) true =  compress tks outtk ((FUN(name,param,commands@[FUN(name1,param1,commands1,env1,typ1)],env,typ))::fs) false
  | compress (FUNEND("","")::tks) outtk (FUN(name1,param1,commands1,env1,typ1)::fs) true =  compress tks (outtk@[FUN(name1,param1,commands1,env1,typ1)]) fs false 
  | compress (RETURN("","")::tks) outtk ((FUN(name,param,commands,env,typ))::fs) ig = compress tks outtk ((FUN(name,param,commands@[RETURN("","")],env,typ))::fs ) true
  | compress (tk::tks) outtk fs true = compress tks outtk fs true
  | compress (tk::tks) outtk ((FUN(name,param,commands,env,typ))::fs) false   = compress tks outtk ((FUN(name,param,commands@[tk],env,typ))::fs ) false
  | compress (tk::tks) outtk funInfo false =  compress tks (outtk@[tk]) funInfo false

fun flatten (x::xs) = x @ flatten(xs)
  | flatten [] = []


fun extract(infile) =
    let
      val is = TextIO.openIn infile   
      fun loop lineOpt =
        case lineOpt of
          SOME line => (implode(List.filter (fn x => x <> #"\n") (explode line))) :: (loop (TextIO.inputLine is))
          | NONE    => []
    in
      loop (TextIO.inputLine is)
    end

fun writeTo outfile s  = 
    let 
      val os = TextIO.openOut outfile
      val o1 = TextIO.output(os,s); 
      val o2 = TextIO.closeOut os;
    in
        ()
    end


fun lookup x (((key,value)::kvlist)::kvlistlist) = if x = key then value else lookup x (kvlist::kvlistlist)
  | lookup x ([]::kvlistlist) = lookup x kvlistlist
  | lookup x [] = x

fun lookupAll (x::xs) e = (lookup x e) :: (lookupAll xs e)
  | lookupAll [] e = []



fun perform [] s e = (s,e)

    | perform (INT(x)::cs) (s::ss) e = perform cs ((INT(x)::s)::ss) e
    | perform (NAME(x)::cs) (s::ss) e = perform cs ((NAME(x)::s)::ss) e
    | perform (STRING(x)::cs) (s::ss) e = perform cs ((STRING(x)::s)::ss) e
    | perform (BOOL(x)::cs) (s::ss) e = perform cs ((BOOL(x)::s)::ss) e
    | perform (ERROR::cs) (s::ss) e = perform cs ((ERROR::s)::ss) e
    | perform (QUIT::cs) s e = (s,e)


    | perform (OP("add")::cs) ((a::b::t)::ss) e = (case ((lookup a e),(lookup b e)) of (INT(x),INT(y)) => perform cs ((INT(y+x)::t)::ss) e | _ => perform cs ((ERROR::a::b::t)::ss) e)
    | perform (OP("sub")::cs) ((a::b::t)::ss) e = (case ((lookup a e),(lookup b e)) of (INT(x),INT(y)) => perform cs ((INT(y-x)::t)::ss) e | _ => perform cs ((ERROR::a::b::t)::ss) e)
    | perform (OP("mul")::cs) ((a::b::t)::ss) e = (case ((lookup a e),(lookup b e)) of (INT(x),INT(y)) => perform cs ((INT(y*x)::t)::ss) e | _ => perform cs ((ERROR::a::b::t)::ss) e)
    | perform (OP("rem")::cs) ((a::b::t)::ss) e = (case ((lookup a e),(lookup b e)) of (INT(0),INT(y)) => perform cs ((ERROR::t)::ss) e |(INT(x),INT(y)) => perform cs ((INT(y mod x)::t)::ss) e | _ => perform cs ((ERROR::a::b::t)::ss) e)
    | perform (OP("div")::cs) ((a::b::t)::ss) e = (case ((lookup a e),(lookup b e)) of (INT(0),INT(y)) => perform cs ((ERROR::t)::ss) e |(INT(x),INT(y)) => perform cs ((INT(y div x)::t)::ss) e | _ => perform cs ((ERROR::a::b::t)::ss) e)
    | perform (OP("equal")::cs) ((a::b::t)::ss) e = (case ((lookup a e),(lookup b e)) of (INT(x),INT(y)) => perform cs ((BOOL(y=x)::t)::ss) e | _ => perform cs ((ERROR::a::b::t)::ss) e)
    | perform (OP("lessThan")::cs) ((a::b::t)::ss) e = (case ((lookup a e),(lookup b e)) of (INT(x),INT(y)) => perform cs ((BOOL(y<x)::t)::ss) e | _ => perform cs ((ERROR::a::b::t)::ss) e)
    | perform (OP("and")::cs) ((a::b::t)::ss) e = (case ((lookup a e),(lookup b e)) of (BOOL(x),BOOL(y)) => perform cs ((BOOL(x andalso y)::t)::ss) e | _ => perform cs ((ERROR::a::b::t)::ss) e)
    | perform (OP("or")::cs) ((a::b::t)::ss) e = (case ((lookup a e),(lookup b e)) of (BOOL(x),BOOL(y)) => perform cs ((BOOL(x orelse y)::t)::ss) e | _ => perform cs ((ERROR::a::b::t)::ss) e)
    | perform (OP("not")::cs) ((a::t)::ss) e = (case (lookup a e) of BOOL(x) => perform cs ((BOOL(not x)::t)::ss) e | _ => perform cs ((ERROR::a::t)::ss) e)
    | perform (OP("neg")::cs) ((a::t)::ss) e = (case (lookup a e) of INT(x) => perform cs ((INT(~1*x)::t)::ss) e | _ => perform cs ((ERROR::a::t)::ss) e)
    | perform (OP("if")::cs) ((a::b::c::t)::ss) e = (case (lookup c e) of BOOL(x) => perform cs (((if x then a else b)::t)::ss) e | _ => perform cs ((ERROR::a::b::c::t)::ss) e)

    | perform (OP("swap")::cs) ((a::b::t)::ss) e = perform cs ((b::a::t)::ss) e 
    | perform (OP("pop")::cs) ((a::t)::ss) e = perform cs (t::ss) e 
    
    | perform (FUN(name,param,commands,env,typ)::cs) (s::ss) e = perform cs ((UNIT::s)::ss) (((NAME(name),(FUN(name,param,commands,flatten(e),typ)))::(hd e))::(tl e))
    | perform (OP("call")::cs) ((a::b::t)::ss) e = (case ((lookup a e),(lookup b e)) of 
        (FUN(name,param,commands,env,typ),ERROR) => perform cs ((ERROR::a::b::t)::ss) e
      |  (FUN(name,param,commands,env,""),arg) => ( case (List.last commands) of
            FUNEND("","") => perform (((dropLast commands)@[FUNEND("","")])@cs)  ([]::t::ss) (((NAME(param),(lookup arg e))::env)::e) (*bind formal parameter in outter environment*)
            | RETURN("","") => perform (((dropLast commands)@[RETURN("","")])@cs)  ([]::t::ss) (((NAME(param),(lookup arg e))::env)::e) (*bind formal parameter in outter environment*)
            | _ => perform cs ((ERROR::a::b::t)::ss) e
            )

      | (FUN(name,param,commands,env,"io"),argValue) => ( case ((List.last commands),b) of
            (FUNEND("",""),NAME(argName)) => perform (((dropLast commands)@[FUNEND(param,argName)])@cs)  ([]::t::ss) (( (NAME(param),argValue)::env)::e)  (*bind formal parameter in outter environment*)
            | (RETURN("",""),NAME(argName)) => perform (((dropLast commands)@[RETURN(param,argName)])@cs)  ([]::t::ss) (( (NAME(param),argValue)::env)::e) (*bind formal parameter in outter environment*)
            | _ => perform cs ((ERROR::a::b::t)::ss) e
            )

      | _ => perform cs ((ERROR::a::b::t)::ss) e)
    | perform (OP("endFun")::cs) (x::y::ss) e = perform cs (y::ss) (tl e)
    | perform (RETURN("","")::cs) (x::y::ss) e = perform cs ((((lookup (hd x) e))::y)::ss) (tl e)
    | perform (FUNEND("","")::cs) (x::y::ss) e = perform cs (y::ss) (tl e)
    | perform (RETURN(loc,ret)::cs) (x::y::ss) (e::f::es) = perform cs ((((lookup (hd x) (e::f::es)))::y)::ss)  (((NAME(ret),(lookup (NAME(loc)) (e::f::es)) )::f)::es)
    | perform (FUNEND(loc,ret)::cs) (x::y::ss) (e::f::es) = perform cs (y::ss)  (((NAME(ret),(lookup (NAME(loc)) (e::f::es)))::f)::es)


    | perform (OP("bind")::cs) ((a::b::t)::ss) e = (case ((lookup a e),b) of 
        (INT(x),NAME(y)) => perform cs ((UNIT::t)::ss) (((NAME(y),INT(x))::(hd e))::(tl e)) 
      | (BOOL(x),NAME(y)) => perform cs ((UNIT::t)::ss) (((NAME(y),BOOL(x))::(hd e))::(tl e))
      | (STRING(x),NAME(y)) => perform cs ((UNIT::t)::ss) (((NAME(y),STRING(x))::(hd e))::(tl e))
      | (UNIT,NAME(y)) => perform cs ((UNIT::t)::ss) (((NAME(y),UNIT)::(hd e))::(tl e))
      | _ => perform cs ((ERROR::a::b::t)::ss) e)
    

    | perform (OP("let")::cs) s e = perform cs ([]::s) ([]::e)
    | perform (OP("end")::cs) (x::y::ss) e = perform cs (((hd x)::y)::ss) (tl e)


    | perform (c::cs) (s::ss) e =  perform cs ((ERROR::s)::ss) e



fun toString (INT(x)) = if (x > ~1) then (Int.toString x) else ("-" ^ Int.toString (abs x))
    | toString (BOOL(x)) = if x then ":true:" else ":false:"
    | toString (NAME(x)) = x
    | toString (STRING(x)) = x
    | toString UNIT = ":unit:"
    | toString ERROR = ":error:"
    | toString _ = "unkown"

fun printStack (x::xs) = toString(x) ^ "\n" ^ (printStack xs)
    | printStack _ = ""

fun interpreter(infile,outfile) = 
    let
        val commands = compress ( map tokenize (extract infile)) [] [] false
        val (s,e) = perform commands [[]] [[]]
        val resultStack = hd (s)
    in
        writeTo outfile (printStack resultStack)
    end


(*Testing phase 1*)

val l = [1,2,3,4,5,6]
val args = map (fn x => 
  ("tests/phase1/input_" ^ Int.toString(x) ^ ".txt" ,
    "tests/phase1/sml/output_" ^ Int.toString(x) ^ ".txt" 
    )) l;

(*Testing phase 2*)

map interpreter args;

val l = [1,2,3,4,5,6,7,8,9,10]
val args = map (fn x => 
  ("tests/phase2/input_" ^ Int.toString(x) ^ ".txt" ,
    "tests/phase2/sml/output_" ^ Int.toString(x) ^ ".txt" 
    )) l;

map interpreter args;

(*Testing phase 3*)

val l = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]
val args = map (fn x => 
  ("tests/phase3/input_" ^ Int.toString(x) ^ ".txt" ,
    "tests/phase3/sml/output_" ^ Int.toString(x) ^ ".txt" 
    )) l;

map interpreter args;





