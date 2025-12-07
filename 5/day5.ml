(* Part 1 *)

let rec check_if_in_ranges curr_range v = function
  | [] -> false
  | h :: t -> 
    let min = fst h in
    let max = snd h in   
    if v >= min && v <= max then true else
    if t = [] then false else
    check_if_in_ranges (List.hd t) v t 

let rec grab_ranges lst = function
  | [] -> lst
  | h :: t -> 
    if h = "" then lst else 
    let spl = String.split_on_char '-' h in
    let min = List.nth spl 0 |> int_of_string in
    let max = List.nth spl 1 |> int_of_string in
    grab_ranges ((min, max) :: lst) t 

let rec grab_values lst = function
  | [] -> lst
  | h :: t -> if h = "" || String.contains h '-' then grab_values lst t else grab_values (int_of_string h :: lst) t

let rec are_values_in_ranges fresh ranges values = match values with
  | [] -> fresh
  | h :: t ->
    let rec is_in_ranges = function
      | [] -> are_values_in_ranges fresh ranges t
      | rh :: rt -> if check_if_in_ranges rh h ranges then are_values_in_ranges (fresh + 1) ranges t else are_values_in_ranges fresh ranges t
    in
    is_in_ranges ranges
    
let part1 l =
  let ranges = grab_ranges [] l in
  let values = grab_values [] l in
  are_values_in_ranges 0 ranges values

(* Part 2 *)

let rec max_number max = function
  | [] -> max
  | h :: t -> if snd h > max then max_number (snd h) t else max_number max t

let rec min_number min = function
  | [] -> min
  | h :: t -> if fst h < min then min_number (fst h) t else min_number min t

let rec generate_values min max = Seq.ints min |> Seq.map (fun x -> x + 1) |> Seq.take max

(*let rec combine_range acc range = function*)

let part2 l =
  let ranges = grab_ranges [] l in
  let max = max_number 0 ranges in
  let min = min_number 0 ranges in
  let values = generate_values min max |> List.of_seq in
  are_values_in_ranges 0 ranges values



