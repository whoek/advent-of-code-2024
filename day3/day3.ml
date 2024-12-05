open Str

(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  In_channel.with_open_bin file
    In_channel.input_all

let data = load input_file

let rec find_all s pos acc  =
  let r = regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}
in
  try
    let find = search_forward r s pos in
    let sum =
      matched_string s
      |> replace_first ( regexp "mul(" ) ""
      |> replace_first ( regexp ")" )  ""
      |> String.split_on_char ','
      |> List.map (fun x -> int_of_string x)
      |> List.fold_left ( * ) 1
    in
    find_all s (find + 1) (acc + sum)
  with Not_found -> acc

let () =
  let result = find_all data 0 0 in
  Printf.printf "\n\nPart 1 - sum of mul:  %i\n" result

(* Part 1 - sum of mul:  183380722 *)


let remove s =
  let r2 =regexp  "don't\\(\\).*?do\\(\\)" in
        global_replace r2 "" s

let () =
  let p2_data = remove data in
  let result = find_all p2_data 0 0 in
  Printf.printf "\n\nPart 2 - sum of do_dont:  %i\n" result



(*   I am still stuck on part II

   18925497 to low
   63298304 to low
   89823704  not correct
   35155827
  153733279

move on to next day....

*)
