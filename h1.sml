fun is_older(x: (int * int * int), y: (int * int * int)) = 
  if (#1 x < #1 y) then 
    true
  else if (#1 x > #1 y) then
    false
  else if (#2 x < #2 y) then
    true
  else if (#2 x > #2 y) then 
    false
  else if (#3 x < #3 y) then 
    true
  else 
    false  

fun number_in_month(dates: (int * int * int) list, month: int): int =
  if (null dates) then 
    0
  else if (#2 (hd dates) = month) then
    1 + number_in_month(tl dates, month)
  else 
    number_in_month(tl dates, month)

fun number_in_months(dates: (int * int * int) list, months: int list): int = 
  if (null months) then
    0
  else 
    number_in_month(dates, hd months) + number_in_months(dates, tl months) 

fun dates_in_month(dates: (int * int * int) list, month: int): (int * int * int) list = 
  if (null dates) then
    []
  else if (#2 (hd dates) = month) then
    (hd dates) :: dates_in_month(tl dates, month)
  else
    dates_in_month(tl dates, month)

fun dates_in_months(dates: (int * int * int) list, months: int list): (int * int * int) list = 
  if (null months) then
    []
  else
    dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strs: string list, index: int): string = 
  if (index = 1) then
    hd strs
  else
    get_nth(tl strs, index - 1)

fun date_to_string(date: (int * int * int)): string = 
  let 
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end 

fun number_before_reaching_sum(sum: int, elems: int list): int = 
  if (sum <= hd elems) then 
    0
  else
    1 + number_before_reaching_sum(sum - (hd elems), tl elems)

fun what_month(day: int): int =
  let
    val daysOfMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]       
  in
    1 + number_before_reaching_sum(day, daysOfMonth)
  end 

fun month_range(day1: int, day2: int): int list =
  if (day1 > day2) then
    []
  else
    what_month day1 :: month_range(day1 + 1, day2)

fun oldest(dates: (int * int * int) list): (int * int * int) option = 
  if (null dates) then
    NONE
  else
    let
      val rest = oldest(tl dates) 
    in
      if (isSome rest) then
        if (is_older(hd dates, valOf rest)) then
          SOME (hd dates)
        else
          rest 
      else
        SOME (hd dates)    
    end

fun remove_duplicate(elems: int list, data: int): int list = 
  if (null elems) then
    [data]
  else if (hd elems = data) then
    remove_duplicate(tl elems, data)
  else
    hd elems :: remove_duplicate(tl elems, data)

fun remove_duplicate_list(elems: int list, datas: int list): int list = 
  if (null datas) then
    elems
  else
    remove_duplicate_list(remove_duplicate(elems, hd datas), tl datas)

fun number_in_months_challenge(dates: (int * int * int) list, months: int list): int =
  number_in_months(dates, remove_duplicate_list(months, months))

fun dates_in_months_challenge(dates: (int * int * int) list, months: int list): (int * int * int) list =
  dates_in_months(dates, remove_duplicate_list(months, months))

fun divisibleBy(x: int, y: int): bool = 
  if x = 0 then
    true
  else if x < 0 then
    false
  else
    divisibleBy(x - y, y)


fun reasonable_date(date: (int * int * int)): bool = 
  let
    val daysOfMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val year = #1 date
    val month = #2 date
    val day = #3 date
    val leap = divisibleBy(year, 400) orelse (divisibleBy(year, 4) andalso not(divisibleBy(year, 100)))
    fun getDaysOfMonth(days: int list, monthIndex: int): int =
      if monthIndex = 1 then
        hd days
      else
        getDaysOfMonth(tl days, monthIndex - 1)
    fun getMaxDay(): int =
      if (month <> 2) then
        getDaysOfMonth(daysOfMonth, month) 
      else if (leap) then
        29
      else
        28
    val maxDay = getMaxDay()
  in  
    year > 0 andalso month >= 1 andalso month <= 12 andalso day >= 1 andalso day <= maxDay 
  end
