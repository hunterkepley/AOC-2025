(* Part 1 *)
let grab_cell grid p =
  if fst p = -1 || snd p = -1 then "empty" else String.make 1 (List.nth grid @@ snd p).[fst p]

let grab_neighbors grid p =
  (* Left *)
  let n1x = if fst p > 0 then fst p - 1 else -1 in
  let n1 = (n1x, snd p) in
  (* Right *)
  let n2x = if fst p < (List.nth grid 0 |> String.length) - 1 then fst p + 1 else -1 in
  let n2 = (n2x, snd p) in
  (* Up *)
  let n3y = if snd p > 0 then snd p - 1 else -1 in
  let n3 = (fst p, n3y) in
  (* Down *)
  let n4y = if snd p < (List.length grid) - 1 then snd p + 1 else -1 in
  let n4 = (fst p, n4y) in
  (* UpLeft *)
  let n5 = (fst n1, snd n3) in
  (* UpRight *)
  let n6 = (fst n2, snd n3) in
  (* DownLeft *)
  let n7 = (fst n1, snd n4) in
  (* DownRight *)
  let n8 = (fst n2, snd n4) in
  let neighbors = [n1; n2; n3; n4; n5; n6; n7; n8] in
  List.map (fun x -> grab_cell grid x) neighbors
  |> List.filter (fun x -> String.length x = 1)

let count_neighbor_rolls grid p =
  let neighbors = grab_neighbors grid p in
  List.fold_left (fun acc x -> if x = "@" then acc + 1 else acc) 0 neighbors

let fewer_than_n_neighbor_rolls n grid p =
  if grab_cell grid p = "@" && count_neighbor_rolls grid p < n then true else false

let get_new_point grid p = if fst p = (List.nth grid 0 |> String.length) - 1 then (0, snd p + 1) else (fst p + 1, snd p) 

let rec check_rolls n acc grid p =
  let v = if fewer_than_n_neighbor_rolls n grid p then acc + 1 else acc in
  let new_p = get_new_point grid p in
  if snd new_p > List.length grid - 1 then v else check_rolls n v grid new_p

let part1 s =
  let grid = String.split_on_char '\n' s in
  check_rolls 4 0 grid (0, 0)

(* Part 2 *)
let rec remove_roll acc s i curr_index =
  let c = if String.length acc = i then "." else String.make 1 s.[curr_index] in
  let new_acc = acc ^ c in
  if curr_index = String.length s - 1 then new_acc else remove_roll new_acc s i (curr_index + 1)

let rec remove_roll_from_grid acc p curr_index = function
  | [] -> acc
  | h :: t -> 
    if curr_index = snd p then 
      remove_roll_from_grid (acc @ [remove_roll "" h (fst p) 0]) p (curr_index + 1) t
    else 
      remove_roll_from_grid (acc @ [h]) p (curr_index + 1) t

let rec check_and_remove_rolls n acc grid p =
  let v = if fewer_than_n_neighbor_rolls n grid p then acc + 1 else acc in
  let new_p = get_new_point grid p in
  let new_grid = if v > acc then remove_roll_from_grid [] p 0 grid else grid in
  if snd new_p > List.length grid - 1 then (v, new_grid) else check_and_remove_rolls n v new_grid new_p 


let part2 s =
  let grid = String.split_on_char '\n' s in
  let rec run_part2_recursively acc grid =
    let v = check_and_remove_rolls 4 0 grid (0, 0) in
    if fst v = 0 then acc else run_part2_recursively (acc + fst v) (snd v)
  in
  run_part2_recursively 0 grid


