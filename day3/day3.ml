open Str

(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  In_channel.with_open_bin file
    In_channel.input_all

let data = load input_file

let r = regexp "mul([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?)"

let rec find_all str pos acc  =
  try
    let find = search_forward r str pos in
    let sum =
      matched_string data
      |> replace_first ( regexp "mul(" ) ""
      |> replace_first ( regexp ")" )  ""
      |> String.split_on_char ','
      |> List.map (fun x -> int_of_string x)
      |> List.fold_left ( * ) 1
    in
    find_all str (find + 1) (acc + sum)
  with Not_found -> acc

let () =
  let result = find_all data 0 0 in
  Printf.printf "Part 1 - sum of mul:  %i\n" result

(* Part 1 - sum of mul:  183380722 *)




(* still busy with part 2 *)


(*  notepad // delete when done

pos 1 = "do()"
pos 2 =  "don't"
pos 4 = "mul"

match

*)
