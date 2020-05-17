fun year_of (date : int * int * int) = #1 date
fun month_of (date : int * int * int) = #2 date
fun day_of (date : int * int * int) = #3 date
val days_list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun is_older (a : (int * int * int), b : (int * int * int)) =
    let
        val ya = year_of a
        val yb = year_of b
        val ma = month_of a
        val mb = month_of b
        val da = day_of a
        val db = day_of b
    in
        ya < yb
        orelse (ya = yb andalso ma < mb)
        orelse (ya = yb andalso ma = mb andalso da < db)
    end

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else
        let val equal = if month_of (hd dates) = month then 1 else 0
        in
            equal + number_in_month (tl dates, month)
        end

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if month_of (hd dates) = month
    then hd dates :: dates_in_month (tl dates, month)
    else dates_in_month (tl dates, month)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month (dates,  hd months) @ dates_in_months (dates, tl months)

fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth (tl strings, n - 1)

fun date_to_string (date : (int * int * int)) =
    let val months = ["January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November",
                      "December"]
    in
        get_nth (months, #2 date) ^ " " ^ (Int.toString (day_of date)) ^ ", " ^
        (Int.toString (year_of date))
    end

fun number_before_reaching_sum (sum : int, xs : int list) =
    let
        fun count (acc : int, xs : int list) =
            if acc + hd xs >= sum
            then 0
            else 1 + count (acc + hd xs, tl xs)
    in
        count (0, xs)
    end

fun what_month (day : int) = number_before_reaching_sum (day, days_list) + 1

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month day1 :: month_range (day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else if null (tl dates)
    then SOME (hd dates)
    else if is_older (hd dates, hd (tl dates))
    then oldest ((hd dates) :: (tl (tl dates)))
    else oldest (tl dates)

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    let
        fun exists_in_list (x : int, xs : int list) =
            if null (xs)
            then false
            else if x <> hd xs
            then exists_in_list (x, tl xs)
            else true
        fun remove_duplicates (xs : int list) =
            if null (tl xs)
            then hd xs :: []
            else if exists_in_list(hd xs, tl xs)
            then remove_duplicates(tl xs)
            else hd xs :: remove_duplicates(tl xs)
        val unique_months = remove_duplicates months
    in
        number_in_months (dates, unique_months)
    end

fun reasonable_date (date : (int * int * int)) =
    let
        val valid_year = (year_of date) > 0
        val valid_month = (month_of date) > 0 andalso (month_of date) <= 12
        val valid_day =
            let
                val leap = year_of date mod 400 = 0
                           orelse ((year_of date) mod 100 <> 0
                                   andalso (year_of date) mod 4 = 0)
                fun get_nth (x : int, xs : int list) =
                    if x = 1
                    then hd xs
                    else get_nth (x - 1, tl xs)
                val days_in_month = if valid_month then get_nth (month_of date, days_list) else 32
            in
                if leap
                then (day_of date) <= 29
                else if month_of date = 2
                then (day_of date) <= 28
                else day_of date <= days_in_month
            end
    in
        valid_year andalso valid_month andalso valid_day
    end
