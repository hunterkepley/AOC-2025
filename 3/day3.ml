(* Part 1: *)
let max_battery r = List.fold_left max 0 r

let rec store_pairs_for_head o acc ignore i = function
  | [] -> acc
  | h :: t -> 
    let new_i = i + 1 in
    if ignore = i then
      acc
    else
      store_pairs_for_head o ((o ^ h) :: acc) ignore new_i t

let rec store_all_pairs acc ignore full = function
  | [] -> acc
  | h :: t ->
    let new_ignore = ignore + 1 in
    let new_acc = (acc @ store_pairs_for_head h [] ignore 0 (List.rev full)) in
    store_all_pairs new_acc new_ignore full t

let convert_to_int_list l = List.map (fun x -> x |> int_of_string) l

let find_max_in_pairs row_pairs =
  max_battery @@ convert_to_int_list row_pairs

let get_pairs_from_row s =
  let b = List.init (String.length s) (String.get s) in
  let l = List.map (fun x -> String.make 1 x) b in
  store_all_pairs [] 0 l (List.rev l)

let rec parse_rows acc = function
  | [] -> acc
  | h :: t ->
    let max = find_max_in_pairs @@ get_pairs_from_row h in
    parse_rows (acc + max) t

let part1 s =
  let l = String.split_on_char '\n' s in
  parse_rows 0 l

(* Part 2 *)

let rec find_first_pos r i s battery =
  if i > s then i else
  if r.[i] = battery then i else 
  find_first_pos r (i + 1) s battery 

let rec find_max r i last curr =
  if i > last then 
    curr
  else 
    let new_curr = max curr r.[i] in
    find_max r (i + 1) last new_curr

let get_n_from_row n r =
  let len = String.length r in
  let rec find_batteries i curr acc =
    if curr = n then acc
    else if i = len then acc
    else if len - i = n - curr then
      acc ^ String.sub r i (len - i)
    else
      let last = len - (n - curr) in
      let max_char = find_max r i last r.[i] in
      let max_pos = find_first_pos r i last max_char in
      find_batteries (max_pos + 1) (curr + 1) (acc ^ String.make 1 r.[max_pos])
  in
  find_batteries 0 0 ""

let part2 s =
  let l = String.split_on_char '\n' s in
  let battery_lst = List.map (fun x -> get_n_from_row 12 x |> int_of_string) l in
  List.fold_left (+) 0 battery_lst
