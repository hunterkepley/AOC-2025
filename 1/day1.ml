let rec perform_rotations curr acc = function
  | [] -> acc
  | h :: t -> match h with
    | "" -> perform_rotations curr acc t (* Purely for sanitizing inputs *)
    | _ ->
      let rot = int_of_string @@ String.sub h 1 ((String.length h) - 1) in
      if String.get h 0 = 'L' then
        let final_rot = if curr - rot < 0 then (curr - rot) mod 100 else curr - rot in
        let new_acc = if final_rot = 0 then acc + 1 else acc in
        perform_rotations final_rot new_acc t
      else 
        let final_rot = if curr + rot > 99 then (curr + rot) mod 100 else curr + rot in
        let new_acc = if final_rot = 0 then acc + 1 else acc in
        perform_rotations final_rot new_acc t

let rec turn curr acc h left =
  if String.get h 0 = 'L' then
    let final_total = if curr - 1 = -1 then 99 else curr - 1 in
    let new_acc = if final_total = 0 then acc + 1 else acc in
    if left - 1 = 0 then
      (new_acc, final_total)
    else
      turn final_total new_acc h (left - 1)
  else
    let final_total = if curr + 1 = 100 then 0 else curr + 1 in
    let new_acc = if final_total = 0 then acc + 1 else acc in
    if left - 1 = 0 then
      (new_acc, final_total)
    else
      turn final_total new_acc h (left - 1)

let rec perform_rotations_part2 curr acc = function
  | [] -> acc
  | h :: t -> match h with
    | "" -> perform_rotations_part2 curr acc t
    | _ ->
      if String.get h 0 = 'L' then
        let rot = int_of_string @@ String.sub h 1 ((String.length h) - 1) in
        let tup = turn curr 0 h rot in
        perform_rotations_part2 (snd tup) (acc + fst tup ) t
      else
        let rot = int_of_string @@ String.sub h 1 ((String.length h) - 1) in
        let tup = turn curr 0 h rot in
        perform_rotations_part2 (snd tup) (acc + fst tup) t

let solve_part1 lst = perform_rotations 50 0 lst
let solve_part2 lst = perform_rotations_part2 50 0 lst

