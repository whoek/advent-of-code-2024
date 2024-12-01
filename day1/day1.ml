(****** BEGIN helper functions ******)

let first l = List.hd l

let last l = List.rev l |> List.hd

(* read lines a file file *)
let lines f =
  let contents = In_channel.with_open_bin f
      In_channel.input_all in
  String.split_on_char '\n' contents

(* how many time does element v occur in list l *)
let occur v l =
  List.find_all (fun x -> x = v) l |> List.length

(****** END helper functions ******)

let file = "input.txt"

(* get data and filter out blank strings -- last one was empty *)
let data =  lines file
            |> List.filter (fun x -> (String.length x) > 0)

(* get left & right value in every string -> convert to INT -> sort list *)

let ll = List.map
    (fun x -> int_of_string @@ first @@ String.split_on_char ' ' x) data
         |>  List.sort compare

let rl = List.map
    (fun x -> int_of_string @@ last @@ String.split_on_char ' ' x) data
         |>  List.sort compare

let diff = List.map2 (fun x y -> abs(x - y)) ll rl

let sum_diff = List.fold_left (+) 0 diff

let () = Printf.printf "Part 1 - sum of diff:  %i\n" sum_diff


let simm = List.map (fun x -> x * occur x rl) ll

let sum_simm = List.fold_left (+) 0 simm

let () = Printf.printf "Part 2 - sum of simm:  %i\n" sum_simm
