(* let input_file = "input.txt" *)
let input_file = "sample.txt"

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
  (* Printf.printf "%i %i    \n%!" i (List.length acc); *)
  if i <= 0 then acc else foldi (pred i) f (f acc)

let part1 =
  foldi 25 blink init

let () = Printf.printf "Part 1 - %i\n" (List.length part1)

(* 207683 *)

(* PART 2 *)


let stones = Hashtbl.create 1000

let find h k = Hashtbl.find h k
let replace h k v  = Hashtbl.replace h k v
let mem  h k = Hashtbl.mem h k
let add_one h k =
  if mem stones k
  then replace h k (1 + find h k)
  else replace h k 1
let remove_one h k =
  replace stones k ((find h k) - 1)

(* load initial values *)
let () = List.iter (fun x ->
    add_one stones x
  ) init


let rec blink_p2 h =
  Hashtbl.iter (fun k v ->
      match k with
      | 0 -> begin
          add_one stones 1;
          remove_one stones 0;
        end
      | k when even_digits k -> begin
          add_one stones (right_half k);
          add_one stones (left_half k);
          remove_one stones k;
        end
      | _ -> begin
    add_one stones (k * 2024);
    remove_one stones k;
  end
    ) stones

(* let () = blink_p2 stones *)

let p2 = Hashtbl.fold (fun k v acc ->
    Printf.printf "k: %i  v: %i \n" k v;
    acc + v
) stones 0

let () = Printf.printf "Part 2 - %i\n" p2


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
