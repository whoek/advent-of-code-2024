(****** START of helper functions ******)

let first lst = List.hd lst

let rec last lst =
  match lst with
  | [] -> failwith "list empty"
  | [x] -> x
  | _::tl -> last tl

(* read file into a list - every line is an element *)
let lines f =
  let contents = In_channel.with_open_bin f
      In_channel.input_all in
  String.split_on_char '\n' contents

(* how many times does element v occur in list lst *)
let occur v lst =
  List.find_all (fun x -> x = v) lst |> List.length

(****** END of helper functions ******)


let file = "input.txt"

(* get data and filter out empty lines -- last one was empty *)
let data =  lines file
            |> List.filter (fun x -> (String.length x) > 0)

(* get left & right values of every string -> convert to INT -> sort list *)
let left = List.map
    (fun x -> String.split_on_char ' ' x |> first |> int_of_string) data
         |>  List.sort compare

let right = List.map
    (fun x -> String.split_on_char ' ' x |> last |> int_of_string) data
         |>  List.sort compare

let () =
  let diff = List.map2 (fun x y -> abs(x - y)) left right in
  let sum_diff = List.fold_left (+) 0 diff in
  Printf.printf "Part 1 - sum of diff:  %i\n" sum_diff

let () =
  let simm = List.map (fun x -> x * occur x right) left in
  let sum_simm = List.fold_left (+) 0 simm in
  Printf.printf "Part 2 - sum of simm:  %i\n" sum_simm
