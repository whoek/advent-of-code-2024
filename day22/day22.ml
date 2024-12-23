(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents

let data =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)
            |> List.map int_of_string

let mix ~secret ~n = secret lxor n
let prune secret = secret mod 16777216

let next secret =
  let calc1 = mix ~n:(secret * 64) ~secret:secret |> prune in
  let calc2 = mix ~n:(calc1 / 32) ~secret:calc1   |> prune in
  let calc3 = mix ~n:(calc2 * 2048) ~secret:calc2 |> prune
  in calc3

(* https://stackoverflow.com/questions/15285386/  *)
let rec foldi i f acc =
  if i <= 0 then acc else foldi (pred i) f (f acc)

let part1 = List.fold_left (fun acc x ->
    acc + foldi 2000 next x
  ) 0 data

let () = Printf.printf "Part 1 - %i\n%!" part1

(* 16999668565 *)

(* ================ part 2 ================*)

(*
 * First 2000 prices for a secret
 * e.g. prices_2000 123 = [3; 0; 6; 5; 4; 4; ...]
 *)
let prices_2000 i =
  let rec prices secret n =
    let secret_price = secret mod 10 in
    match n with
    | 0 -> []
    | _ -> secret_price :: prices (next secret) (n - 1)
  in prices i 2000

(*
 * Get all prices and sequences for all monkies
 * int list -> (int * string) list
 * e.g. [[(1, "42-81"); (9, "2-818"); ....
 *)
let rec seq lst =
  match lst with
  | e1 :: e2 :: e3 :: e4 :: e5 :: tl -> begin
      let seq_str = string_of_int (e2 - e1) ^
                    string_of_int (e3 - e2) ^
                    string_of_int (e4 - e3) ^
                    string_of_int (e5 - e4) in
      (e5, seq_str) :: seq (e2 :: e3 :: e4 :: e5 :: tl)
    end
  | _ -> []

let price_and_seq = List.map (fun x ->
    prices_2000 x
    |> seq
  ) data

(* get list of ALL unique 4 number sequences - 40_951 *)
let uniq_seq  =
  List.map (fun x ->
      List.map (fun (p, s) -> s) x
    ) price_and_seq
  |> List.flatten
  |> List.sort_uniq compare

(*
 * Calculate total bananas for every unique sequence
 * this is HIGHLY inefficient: 1hr runtime but right answer
 *)
let check_all =
  let compare_points (_, a2) (_, b2) =
    compare  b2 a2 in
  let rec check  seq l =
    match l with
    | (p, s) :: _ when s = seq -> p
    |  _ :: tl -> check seq tl
    | [] -> 0
  in
  List.mapi (fun i x -> x, (
      Printf.printf "%i \n%!" i;
      List.map (fun y -> check x y) price_and_seq
      |> List.fold_left ( + ) 0
    )
    ) uniq_seq
  |> List.sort compare_points

let () =
  let seq, bananas = List.hd check_all in
  Printf.printf "\npart 2:  %s  -> %i \n%!" seq bananas

(* part2 = 1898 bananas -- runtime:61m54  :)     *)
