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

(* Part 2 - REALLY SLOW WAY, takes a super long time *)
let generate_values checklist min max =
  Seq.iterate succ (succ min) |> Seq.take (max - min)
  |> Seq.map (fun x -> if Seq.exists ((=) x) checklist then -1 else x)
  |> Seq.filter ((<>) ~-1)

let rec unpack_ranges acc = function
  | [] -> acc
  | h :: t ->
    unpack_ranges (Seq.append (generate_values acc (fst h) (snd h)) acc) t

let part2_slow l =
  let ranges = grab_ranges [] l in
  let unique = unpack_ranges Seq.empty ranges in
  Seq.length unique

(* Part 2 - Solution which actually finishes in a lifetime *)
let combine_ranges ranges =
    let sorted = List.sort (fun x y -> compare (fst x) (fst y)) ranges in
    let rec combine acc = function
      | [] -> List.rev acc
      | h :: t ->
          match acc with
          | [] -> combine [h] t
          | last :: acc_rest ->
              if (fst h) <= (snd last) + 1 then
                let combined = (fst last, max (snd last) (snd h)) in
                combine (combined :: acc_rest) t 
              else
                combine (h :: acc) t
    in
    combine [] sorted

let part2 l =
  let ranges = grab_ranges [] l in
  let combined_ranges = combine_ranges ranges in
  combined_ranges
  |> List.fold_left (fun acc x -> acc + ((snd x) - (fst x))) 0
  |> (+) (List.length combined_ranges)

