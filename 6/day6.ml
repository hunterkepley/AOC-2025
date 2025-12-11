(* Part 1 *)
let create_chunk s =
  String.to_seq s
  |> Seq.fold_lefti (fun acc i x -> 
    if i + 1 >= String.length s then 
      if x = ' ' then acc else Seq.cons x acc 
    else 
      if x = ' ' && s.[i + 1] = ' ' then acc else 
      Seq.cons x acc) Seq.empty
  |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq |> String.trim |> String.split_on_char ' '

let rec create_problem_list acc = function
  | [] -> acc
  | h :: t ->
    let chunk = create_chunk h in
    if acc = [] then create_problem_list (List.map (fun x -> [x]) chunk) t else
    create_problem_list (List.mapi (fun i x -> List.nth chunk i :: x) acc) t

let safe_mul x y = if y = 0 then x else x * y

let rec solve_problems acc = function
  | [] -> acc
  | h :: t -> match h with
    | [] -> acc
    | op :: nums ->
      let _nums = List.map (fun x -> int_of_string @@ String.trim x) nums in
      let ( !! ) = match op with
        | "*" -> ( safe_mul )
        | _ -> ( + )
      in
    solve_problems (acc + (List.fold_left (fun n x -> !! x n) 0 _nums)) t

let part1 s =
  String.split_on_char '\n' s
  |> create_problem_list []
  |> solve_problems 0
  
(* Part 2 *)
(* Just need to parse the problems differently, the solver will still work *)
let create_list_of_string s = String.(List.init (length s) (get s))

let string_of_char c = String.make 1 c

let ( $ ) = string_of_char

let rev_string s = create_list_of_string s |> List.rev |> List.to_seq |> String.of_seq

let ( %% ) = rev_string

let rec hor_lines acc = function
  | [] -> acc
  | h :: t ->
    let rec parse_chunk new_acc = function
      | [] -> new_acc
      | c :: rest -> parse_chunk (c :: new_acc) rest
    in
    let hor_chunk = parse_chunk [] (create_list_of_string h) in
    hor_lines (List.map2 (fun x y -> ($) x ^ y) hor_chunk acc) t

let rec create_hor_problems curr_prob acc = function
  | [] -> (curr_prob :: acc)
  | h :: t ->
    let _h = String.trim h in
    if _h = "" then create_hor_problems [] (curr_prob :: acc) t else
    if _h.[0] = '+' || _h.[0] = '*' then
      let op = ($) _h.[0] in
      let num_str = String.sub _h 1 (String.length _h - 1) in
      create_hor_problems (op :: (%%) num_str :: curr_prob) acc t 
    else
      create_hor_problems ((%%) _h :: curr_prob) acc t 
      

let part2 s =
  let lines = String.split_on_char '\n' s in
  let init_acc = List.init (String.length @@ List.nth lines 0) (fun _ -> "") in
  hor_lines init_acc lines
  |> create_hor_problems [] []
  |> solve_problems 0

