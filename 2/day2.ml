let rec first_invalid_range min max = 
  let _min = int_of_string min in
  let _max = int_of_string max in
  if _min = _max + 2 then (false, _min) else
  if (String.length min) mod 2 != 0 then first_invalid_range (_min + 1 |> string_of_int) max else
    let len = String.length min in
    let half_len = len / 2 in
    if String.sub min 0 (half_len) = String.sub min (half_len) (half_len) then
      (true, _min)
    else
      first_invalid_range (_min + 1 |> string_of_int) max

let rec check_range min max acc =
  let _min = int_of_string min in
  let _max = int_of_string max in
  let r = first_invalid_range min max in
  let new_acc = if fst r then acc + snd r else acc in
  if snd r < _max + 1 then 
    check_range (snd r + 1 |> string_of_int) max new_acc
  else
    acc

let rec validate_inputs acc = function
  | [] -> acc
  | h :: t ->
    let range = String.split_on_char '-' h in
    match range with
    | min :: max :: _ ->
      let r = check_range min max 0 in
      let new_acc = acc + r in
      if r > 0 then validate_inputs new_acc t else validate_inputs acc t
    | _ ->
      acc

let part1 input =
  validate_inputs 0 @@ String.split_on_char ',' input

let rec find_any_valid min max acc r l =
  let _min = int_of_string min in
  let _max = int_of_string max in
  let new_min = _min + 1 |> string_of_int in
  if _min > _max then acc else
  if l + r > (String.length min) || r > (String.length min) / 2 then find_any_valid new_min max acc 1 0 else
  let comp = String.sub min l r in
  if comp != min && String.sub min 0 r = comp then (* Invalid (matches pattern) *)
    if l + r > String.length min - 1 then (* Finished and invalid *)
      find_any_valid new_min max (acc + _min) 1 0
    else (* Need to continue checking same min *)
      find_any_valid min max acc r (l + r)
  else (* Valid (doesn't match pattern) *)
    if r + l > (String.length min) - 1 then (* Finished checking *)
      find_any_valid new_min max acc 1 0
    else (* Need to continue checking same min *)
      find_any_valid min max acc (r + 1) 0
      

let rec validate_inputs2 acc = function
  | [] -> acc
  | h :: t ->
    let range = String.split_on_char '-' h in
    match range with
    | min :: max :: _ ->
      let new_acc = acc + find_any_valid min max 0 1 0 in
      validate_inputs2 new_acc t
    | _ ->
      acc
   
let part2 input =
  validate_inputs2 0 @@ String.split_on_char ',' input

