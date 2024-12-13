let input_file = "input.txt"
(* let input_file = "sample.txt" *)

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  contents

let init = load input_file
           |> String.trim
           |> String.split_on_char ' '
           |> List.map int_of_string

let even_digits x =
  let n = String.length @@ string_of_int x in
  match n with
  | n when n mod 2 = 0 -> true
  | _ -> false

let left_half x =
  let s = string_of_int x in
  let n = String.length s in
  String.sub s 0 (n/2) |> int_of_string

let right_half x =
  let s = string_of_int x in
  let n = String.length s in
  String.sub s (n/2) (n/2) |> int_of_string

let rec blink lst =
  match lst with
  | 0 :: tl -> 1 :: blink tl
  | hd :: tl when even_digits hd  -> left_half hd :: right_half hd :: blink tl
  | hd :: tl -> (hd * 2024) :: blink tl
  | [] -> []

(* https://stackoverflow.com/questions/15285386/  *)
let rec foldi i f acc =
  Printf.printf "%i %i    \n%!" i (List.length acc);
  if i <= 0 then acc else foldi (pred i) f (f acc)

let part1 =
  foldi 25 blink init


let () = Printf.printf "Part 1 - %i\n" (List.length part1)

(* 207683 *)



(* PART 2 *)

(* check how many unique numers used in PART 1 *)
(* https://www.thekerneltrip.com/ocamli-snippets/count-unique-elements-in-list-ocaml/ *)
let count_unique_elements_naive list =
  let count_element e list = List.filter (fun x -> x = e) list |> List.length in
  List.sort_uniq Int.compare list
  |> List.map (fun e -> (e, count_element e list))
  |> List.length



(*  NOTES

The part 1 method is to slow for part 2
For PART 1 -- although 207k stones -- only 467 unique numbers were useda fter  25 BLINKS
There are LOTS of duplication of numbers

TODO -- Make following change
- Don't use LIST with all the numbers
- The order of stones is irrelivant.
- Keep a HASHTABL with all the numbers on stones
- When BLINK -- simply run through the active numbers and update the rules

*)
