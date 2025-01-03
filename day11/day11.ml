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
  Printf.printf "n:%i \n" i;
  if i <= 0 then acc else foldi (pred i) f (f acc)

let part1 =
  foldi 10 blink init

let () = Printf.printf "Part 1 - %i\n" (List.length part1)

(* 207683 *)

(* PART 2 *)

let find h k = Hashtbl.find h k
let replace h k v  = Hashtbl.replace h k v
let mem  h k = Hashtbl.mem h k
let remove h k = Hashtbl.remove h k
let copy h = Hashtbl.copy h
let add_n h k n =
  if mem h k
  then replace h k @@ n + find h k
  else replace h k n
let remove_n h k n =
  replace h k @@ (find h k) - n

let stones = Hashtbl.create 1000

(* load initial values *)
let () = List.iter (fun x ->
    add_n stones x 1
  ) init

let rec blink_p2 h =
  let h' = copy h in
  Hashtbl.iter (fun k v ->
      (* Printf.printf "k:%i, v:%i \n" k v; *)
      match k with
      | 0 when v > 0 -> begin
          remove h 0 ;
          add_n h 1 v;


        end
      | k when v > 0 && even_digits k -> begin
          remove h k;
          add_n h (right_half k) v;
          add_n h (left_half k) v;


        end
      | _ when v > 0 -> begin
          remove h k;
          add_n h (k * 2024) v;
        end
      | _ when v = 0 -> remove h k
      | _ -> failwith "Invalid state"
    ) h';
  h

let part2 =
  foldi 25 blink_p2 stones

let p2 = Hashtbl.fold (fun k v acc ->
    acc + v
  ) stones 0

let () = Printf.printf "Part 2 - %i\n" p2

(*  NOTES

    The part 1 method is to slow for part 2
    For PART 1 -- although 207k stones -- only 467 unique numbers were useda fter  25 BLINKS


*)
