(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals(strs: string list): string list =
    List.filter (fn c => Char.isUpper(String.sub(c, 0))) strs 

fun longest_string1(strs: string list): string = 
    List.foldl (fn (curr, acc) => if (String.size(curr) > String.size(acc)) then curr else acc) "" strs

fun longest_string2(strs: string list): string =
    List.foldl (fn (curr, acc) => if (String.size(curr) >= String.size(acc)) then curr else acc) "" strs

fun longest_string_helper predict data =
    List.foldl (fn (curr, acc) => if predict(String.size(curr), String.size(acc)) then curr else acc) "" data 

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals 

val rev_string = String.implode o rev o String.explode 

fun first_answer f data =
    case data of
        [] => raise NoAnswer
        | head :: tail => case f(head) of 
            SOME v => v
            | NONE => first_answer f tail

fun all_answers f data = 
    let
        fun merge (left, acc) =
            case (left, acc) of
                ([], _) => acc
                | (head :: tail, SOME v) =>
                    ( case f head of 
                        NONE => NONE
                        | SOME x => merge(tail, SOME (x @ v))  
                    )
                | _ => NONE 
    in
        merge(data, SOME [])
    end

fun count_wildcards(par: pattern): int = g (fn _ => 1) (fn _ => 0) par 

fun count_wild_and_variable_lengths(par: pattern) = g (fn _ => 1) String.size par

fun count_some_var(str: string, par: pattern) = g (fn _ => 0) (fn x => if x = str then 1 else 0) par

fun check_pat(par: pattern): bool = 
    let
        fun walk(p: pattern): string list =
            case p of
                Variable x => [x]
                | TupleP ps => List.foldl (fn (y, x) => x @ (walk y)) [] ps
                | ConstructorP(_,p) => walk p
                | _ => []
        fun hasRepeats(data: string list): bool =
            case data of 
                [] => false
                | head :: tail => (List.exists (fn x => x = head) tail) orelse hasRepeats(tail)
    in
        not(hasRepeats(walk(par)))
    end

fun match(v: valu, par: pattern): (string * valu) list option = 
    case par of 
        Wildcard => SOME []
        | Variable s => SOME [(s, v)]
        | UnitP => (case v of
            Unit => SOME []
            | _ => NONE)
        | ConstP x => (case v of
            Const y => if x = y then SOME [] else NONE
            | _ => NONE)
        | TupleP ps => (case v of
            Tuple vs => if List.length ps = List.length vs then all_answers match (ListPair.zip(vs, ps)) else NONE
            | _ => NONE)
        | ConstructorP (s1, ps) => (case v of
            Constructor (s2, vs) => if s1 = s2 then match(vs, ps) else NONE
            | _ => NONE)

fun first_match v parList = 
    SOME (first_answer (fn p => match(v, p)) parList) handle
        NoAnswer => NONE 
