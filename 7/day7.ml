(* Part 1 *)
let explode_string s = String.(List.init (length s) (get s))

let safe_tl l = if List.length l = 0 then [] else List.tl l

let rec tachyon_line n acc i prev_line = function
  | [] -> (n, acc)
  | h :: t ->
    if i >= List.length prev_line then (n, acc) else
    begin match h with
    | '.' -> 
      if List.nth prev_line i = 'S' || List.nth prev_line i = '|' then 
        tachyon_line n ('|' :: acc) (i + 1) prev_line t 
      else
        tachyon_line n ('.' :: acc) (i + 1) prev_line t
    | '^' ->
      let new_acc = List.tl acc in
        if List.nth prev_line i = '|' then
          tachyon_line (n + 1) ('|' :: '^' :: '|' :: new_acc) (i + 2) prev_line (safe_tl t)
        else
          tachyon_line n ('^' :: acc) (i + 1) prev_line t
    | _ -> tachyon_line n (h :: acc) (i + 1) prev_line t
    end

let part1 s =
  let lines = String.split_on_char '\n' s in
  let split_lines = List.map (fun x -> explode_string x) lines in
  let rec perform_tachyon (n, acc) = function
    | [] -> (n, acc)
    | h :: t -> 
      if acc = [] then 
        perform_tachyon (n, h :: acc) t
      else
        let l = tachyon_line n [] 0 (List.hd acc |> List.rev) h in
        perform_tachyon (fst l, snd l :: acc) t
  in
  let ans = fst @@ perform_tachyon (0, []) split_lines in
  print_endline @@ string_of_int ans
  
