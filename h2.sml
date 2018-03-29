(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun all_except_option(str: string, str_list: string list): (string list) option =
  let 
    fun local_helper(data: string, datas: string list, flag: bool, res: string list) = 
      case datas of [] => if (flag) then SOME res else NONE
      | head :: tail => if (same_string(head, data)) then local_helper(data, tail, true, res) else local_helper(data, tail, flag, res @ [head])
  in
    local_helper(str, str_list, false, [])
  end 

fun get_substitutions1(strs: string list list, str: string): string list = 
  case strs of [] => []
  | head :: tail => 
    case all_except_option(str, head) of NONE => get_substitutions1(tail, str)
    | SOME l => l @ get_substitutions1(tail, str)

fun get_substitutions2(strs: string list list, str: string): string list = 
  let
    fun local_helper(datas: string list list, data: string, res: string list) = 
      case datas of [] => res
      | head :: tail =>
        case all_except_option(data, head) of NONE => local_helper(tail, data, res)
        | SOME l => local_helper(tail, data, res @ l) 
  in
    local_helper(strs, str, [])
  end

fun similar_names(strs: string list list, {first = x : string, middle = y: string, last = z: string}): {first: string, middle: string, last: string} list =
  let
    val subs = get_substitutions2(strs, x)
    fun local_helper(names: string list, res: {first: string, middle: string, last: string} list) = 
      case names of [] => res
      | head :: tail => local_helper(tail,  res @ [{first = head, middle = y, last = z}])
  in
    {first = x, middle = y, last = z} :: local_helper(subs, []) 
  end

fun card_color(c: card) =
  case c of (Clubs, _) => Black 
  | (Spades, _) => Black
  | _ => Red 

fun card_value(c: card) =
  case c of (_, Num x) => x 
  | (_, Ace) => 11
  | _ => 10

fun remove_card(cs: card list, c: card, e: exn) =
  case cs of [] => raise e
  | head :: tail => if c = head then tail else head :: remove_card(tail, c, e)

fun all_same_color(cs: card list) = 
  case cs of [] => true
  | head :: tail =>
    case tail of [] => true
    | headOfTail :: rest => 
      all_same_color(tail) andalso card_color(head) = card_color(headOfTail) 

fun sum_cards(cs: card list) =
  let
    fun sum(cards: card list, res: int) =
      case cards of [] => res
      | head :: tail => sum(tail, res + card_value(head))
  in
    sum(cs, 0)
  end

fun score(cs: card list, target: int) =
  let
    val sum = sum_cards(cs)
    val score = if (sum > target) then 3 * (sum - target) else target - sum
    val same = all_same_color(cs)
  in
    if (same) then score div 2 else score 
  end

fun officiate(cs: card list, mv: move list, goal: int) = 
  let 
    fun run(holds: card list, cards: card list, moves: move list) = 
      case moves of [] => score(holds, goal)
      | moveHead :: moveTail =>
        case moveHead of Discard c => run(remove_card(holds, c, IllegalMove), cards, moveTail)
        | Draw => 
          case cards of [] => score(holds, goal)
          | draw :: tail => if (sum_cards(draw :: holds) > goal) then score(draw :: holds, goal) else run(draw :: holds, tail, moveTail)
  in
    run([], cs, mv)
  end

fun score_challenge(cs: card list, target: int) = 
  let
    val sum = sum_cards(cs)
    fun tryDesc(cs: card list, score: int) =
      case cs of [] => score
      | head :: tail =>
        case head of (_, Ace) => if (score - 10 >= target) then tryDesc(tail, score - 10) else score
        | _ => tryDesc(tail, score)     
    val newScore = tryDesc(cs, sum)
    val score = if (newScore > target) then 3 * (newScore - target) else target - newScore 
    val same = all_same_color(cs)
  in
    if (same) then score div 2 else score 
  end

fun officiate_challenge(cs: card list, mv: move list, goal: int) = 
  let 
    fun run(holds: card list, cards: card list, moves: move list) = 
      case moves of [] => score_challenge(holds, goal)
      | moveHead :: moveTail =>
        case moveHead of Discard c => run(remove_card(holds, c, IllegalMove), cards, moveTail)
        | Draw => 
          case cards of [] => score_challenge(holds, goal)
          | draw :: tail => if (sum_cards(draw :: holds) > goal) then score_challenge(draw :: holds, goal) else run(draw :: holds, tail, moveTail)
  in
    run([], cs, mv)
  end
