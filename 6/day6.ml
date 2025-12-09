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
      let _nums = List.map (fun x -> int_of_string x) nums in
      let ( !! ) = match op with
        | "*" -> ( safe_mul )
        | _ -> ( + )
      in
    solve_problems (acc + (List.fold_left (fun n x -> !! x n) 0 _nums)) t

let parse_input s =
  let lines = String.split_on_char '\n' s in
  create_problem_list [] lines
  |> solve_problems 0
  
